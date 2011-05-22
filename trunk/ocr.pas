unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, tessintf;

type

   TConfidenceList = array of PInteger;

  { TTesseractPage }

  TTesseractPage = class(TObject)
  private
    FPageImage: PLPix;
    FTesseract: TTesseract;
    FBoxes: PBoxArrayArray;
    FConfidenceRating: TConfidenceList;
    FText: String;
  public
    constructor Create( PixIn: PLPix );
    destructor Destroy; override;
    function RecognizeRect( inRect: TRect ): integer;
    property Text: string read FText;
    property WordBoxes: PBoxArrayArray read FBoxes;
    property WordConfidence: TConfidenceList read FConfidenceRating;
  end;

const
  BIN_THRESHOLD = 150;

implementation

uses LibLeptUtils;

{ TTesseractPage }

constructor TTesseractPage.Create ( PixIn: PLPix ) ;
begin
  if pixGetDepth(PixIn) > 1
   then FPageImage := pixThresholdToBinary(PixIn, BIN_THRESHOLD)
   else FPageImage := pixClone(PixIn);
  FTesseract := tesseract_new(nil, nil);
  if FTesseract=nil then writeln('tessearct_new failed :(');
//  tesseract_SetImage(t, ICanvas.Picture);
  tesseract_SetImage(FTesseract, FPageImage);
  FBoxes := boxaaCreate(0);
  SetLength(FConfidenceRating, 20);
end;

destructor TTesseractPage.Destroy;
begin
  if FPageImage<>nil then pixDestroy(@FPageImage);
  tesseract_destroy(FTesseract);
  boxaaDestroy(@FBoxes);
  inherited Destroy;
end;

function TTesseractPage.RecognizeRect ( inRect: TRect ) : integer;
var
  pa: PPixArray;
  ba: PBoxArray;
  wa: PBoxArray;
  na: PNumArray;
  x: Integer;
  lineindex: Single;
  linecount: Integer;
  ra : array[0..255] of TRect;
  n: Integer;
  R: TRect;
  TextRegion: PLPix;
  conf: PInteger;
begin
    if FPageImage=nil then Exit;
    TextRegion := CropPix(FPageImage, inRect);
    writeln( pixGetWordsInTextlines(TextRegion, 1, 20, 20, 200, 500, @ba, @pa, @na) );
    writeln('Boxarray.n: ', ba^.n);
    linecount := 0;

     // --------- loop through word boxes to construct line boxes --------
    n := 0;
    while n<ba^.n do
       begin
       Inc(linecount);
       R.Left := 20;
       R.Top := ba^.box[n]^.h + ba^.box[n]^.y;
       while na^.numarray[n]=lineindex do
             begin
               if ba^.box[n]^.y < R.Top then R.Top := ba^.box[n]^.y;
               if ba^.box[n]^.h + ba^.box[n]^.y > R.Bottom then R.Bottom := ba^.box[n]^.h + ba^.box[n]^.y;
               if ba^.box[n]^.x + ba^.box[n]^.w > R.Right then R.Right := ba^.box[n]^.x + ba^.box[n]^.w;
               Inc(n);
             end;
       x := R.Bottom-R.Top;
       x := 0;
       R.Top := R.Top-x;
       R.Bottom := R.Bottom+x;
       x := trunc(lineindex);
       ra[x] := R;
       lineindex := na^.numarray[n];
       writeln( 'Lineindex: ', FormatFloat('0.00', lineindex));
       end;

     writeln('Linecount: ', linecount);

      // --------- OCR inRect one line at a time to allow for progress callback --------
     for x := 0 to linecount-1 do
          begin
            R := ra[x];
            if (R.Right>R.Left) and (R.Bottom>R.Top) then
            begin
            writeln ('ocr line ', x);
            tesseract_SetRectangle(FTesseract, inRect.Left + R.Left,
                                               inRect.Top + R.Top,
                                               R.Right-R.Left,
                                               R.Bottom-R.Top);
            FText := Ftext + tesseract_GetUTF8Text(FTesseract);
            n := length(Ftext);
            if FText[n]=#10 then SetLength(FText, n-1);
            wa := tesseract_GetWords(FTesseract, nil);

   writeln('Tesseract wordcount: ', boxaGetCount(wa));
   conf := tesseract_AllWordConfidences(FTesseract);

   writeln( boxaWrite(Pchar('/tmp/tessboxes' + IntToStr(x) + '.txt'), wa));
   for n := 0 to wa^.n-1 do writeln('confidence ', conf[n]);

   boxaaAddBoxa(FBoxes, wa, 0);
   if boxaaGetCount(FBoxes) > Length(FConfidenceRating) then SetLength(FConfidenceRating, Length(FConfidenceRating)+20);
   FConfidenceRating[boxaaGetCount(FBoxes)] := conf;
          end
            else writeln('empty box: ', x);
         end;


     {for x := 0 to pa^.n-1 do
          begin
            //pixWrite(Pchar('/tmp/test'+IntToStr(x) +'.bmp'), pa^.pix[x], IFF_BMP);
            //tesseract_SetImage(t, pa^.pix[x]);
            //tesseract_SetRectangle(t, ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w, ba^.box[x]^.h);
            ICanvas.AddSelection( Rect(ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w + ba^.box[x]^.x, ba^.box[x]^.h + ba^.box[x]^.y) );
            //Write(tesseract_GetUTF8Text(t), #32);
          end;  }


    boxDestroy(@ba);
    ptaDestroy(@pa);
    numaDestroy(@na);
    pixDestroy(@TextRegion);

    writeln('Fboxes linecount: ', boxaaGetCount(FBoxes));
    writeln('FConfidenceRating sample: ', FConfidenceRating[2][0]);
end;

end.

