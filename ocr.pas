unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, LibLeptUtils, tessintf;

type

   TConfidenceList = array of PInteger;

  { TTesseractPage }

  TTesseractPage = class(TObject)
  private
    FOnOCRLine: TProgressCallback;
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
    property OnOCRLine: TProgressCallback read FOnOCRLine write FOnOCRLine;
  end;

const
  BIN_THRESHOLD = 150;

implementation

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
  lineindex: integer;
  linecount: Integer;
  ra : array[0..255] of TRect;
  n: Integer;
  R: TRect;
  TextRegion: PLPix;
  conf: PInteger;
  y: LongInt;
  z: Integer;
  b: PLBox;
  wb: PLBox;
  boxCount: LongInt;
  lineNum: Longint;
  wbx, wby, wbw, wbh: Longint;

  procedure AdjustToArea( abox: PLBox; lineref: integer );
  var
    xx, yy: Longint;
  begin
    if boxGetGeometry(abox, @xx, @yy, nil, nil)=0 then
       begin
         xx := inRect.Left + ra[lineref].Left + xx;
         yy := inRect.Top + ra[lineref].Top + yy;
         boxSetGeometry(abox, xx, yy, -1, -1);
       end;
  end;

  function RectIsValid( wordRect: TRect ): Boolean;
  begin
    Result := true;
    if (wordRect.Left<0) then Result := false
     else if (wordRect.Top<0) then Result := false
     else if (wordRect.Right<wordRect.Left) or (wordRect.Right>inRect.Right) then Result := false
     else if (wordRect.Bottom<wordRect.Top) or (wordRect.Bottom>inRect.Bottom) then Result := false;
  end;

begin
    if FPageImage=nil then Exit;
    TextRegion := CropPix(FPageImage, inRect);
    writeln('Created TextRegion');
    pixWrite('/tmp/textregion.bmp', TextRegion, IFF_BMP);
    writeln(Format('inRect: %d %d %d %d', [inrect.Left, inRect.Top, inRect.Right, inRect.Bottom]));
    writeln( pixGetWordsInTextlines(TextRegion, 1, 20, 20, 200, 500, @ba, @pa, @na) );
    writeln('Boxarray.Getcount: ', boxaGetCount(ba));
    linecount := 0;

     // --------- loop through word boxes to construct line boxes --------
    n := 0;
    boxCount := boxaGetCount(ba);
    lineindex := 0;
    while numaGetIValue(na, n, @linenum)=0 do
       begin
         Inc(linecount);
         R.Left := 20;
         R.Top := inRect.Bottom;
         while lineNum=lineindex do
               begin
                 wb := boxaGetBox(ba, n, L_CLONE);
                 boxGetGeometry(wb, @wbx, @wby, @wbw, @wbh);
                 if wby < R.Top then R.Top := wby;
                 if wbh + wby > R.Bottom then R.Bottom := wbh + wby;
                 if wbx + wbw > R.Right then R.Right := wbx + wbw;
                 boxDestroy(@wb);
                 Inc(n);
                 numaGetIValue(na, n, @lineNum);
               end;
        { x := R.Bottom-R.Top;
         x := 0;
         R.Top := R.Top-x;
         R.Bottom := R.Bottom+x; }
         //x := lineindex;
         ra[lineindex] := R;
         lineindex := lineNum;
         writeln( 'Lineindex: ', FormatFloat('0.00', lineindex));
       end;

     writeln('Linecount: ', linecount);
     for x := 0 to pixaGetCount(pa)-1 do
          begin
            //pixWrite(Pchar('/tmp/test'+IntToStr(x) +'.bmp'), pa^.pix[x], IFF_BMP);
            //tesseract_SetImage(t, pa^.pix[x]);
            //tesseract_SetRectangle(t, ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w, ba^.box[x]^.h);
            //ICanvas.AddSelection( Rect(ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w + ba^.box[x]^.x, ba^.box[x]^.h + ba^.box[x]^.y) );
            //Write(tesseract_GetUTF8Text(t), #32);
          end;
     SetLength(FConfidenceRating, linecount+1); //TODO: work out why linecount+0 causes crash
      // --------- OCR inRect one line at a time to allow for progress callback --------
     for x := 0 to linecount-1 do
          begin
            wa := nil;
            R := ra[x];
            if RectIsValid(R) then
            begin
            writeln ('ocr line ', x);
            writeln(Format('line Rect: %d %d %d %d', [R.Left, R.Top, R.Right, R.Bottom]));
            tesseract_SetRectangle(FTesseract, inRect.Left + R.Left,
                                               inRect.Top + R.Top,
                                               R.Right-R.Left,
                                               R.Bottom-R.Top);
            writeln('SetRectangle done');
            try
            FText := Ftext + tesseract_GetUTF8Text(FTesseract);
            writeln('GetUTF8Text done');

            except
              writeln('error in GetUTF8Text');
            end;
            n := length(Ftext);
            if FText[n]=#10 then SetLength(FText, n-1);
            wa := tesseract_GetWords(FTesseract, nil);
            if wa<>nil then
                begin
                   writeln('Tesseract wordcount: ', boxaGetCount(wa));
                   conf := tesseract_AllWordConfidences(FTesseract);

                   writeln( boxaWrite(Pchar('/tmp/tessboxes' + IntToStr(x) + '.txt'), wa));
                   for n := 0 to boxaGetCount(wa)-1 do
                        begin
                          writeln('confidence ', conf[n]);
                          b := boxaGetBox(wa, n, L_COPY);
                          if b<>nil then
                          begin
                            AdjustToArea(b, x);
                            boxaReplaceBox(wa, n, b);
                          end;
                        end;
                   writeln( boxaWrite(Pchar('/tmp/adjusted-tessboxes' + IntToStr(x) + '.txt'), wa));

                   boxaaAddBoxa(FBoxes, wa, 0);
                   y := boxaaGetCount(FBoxes);
                   z := Length(FConfidenceRating);
                   //FConfidenceRating[boxaaGetCount(FBoxes)] := conf;
                end;
            if Assigned(FOnOCRLine)
               then FOnOCRLine(1);
          end
            else writeln('empty box: ', x);
         end;




    boxDestroy(@ba);
    ptaDestroy(@pa);
    numaDestroy(@na);
    pixDestroy(@TextRegion);

    writeln('Fboxes linecount: ', boxaaGetCount(FBoxes));
   // writeln('FConfidenceRating sample: ', FConfidenceRating[2][0]);
end;

end.

