unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, LibLeptUtils, tessintf;

type

   TConfidenceList = array of PInteger;

   TWordData = record
     Start: Integer;
     Length: Integer;
     Box: TRect;
     Confidence: Byte;
   end;

   TLineData = record
     Index: Integer;
     Box: TRect;
     Words: Array of TWordData;
   end;

  { TTesseractPage }

  TTesseractPage = class(TObject)
  private
    FOnOCRLine: TProgressCallback;
    FPageImage: PLPix;
    FTesseract: TTesseract;
    FBoxes: PBoxArrayArray;
    FConfidenceRating: TConfidenceList;
    FText: String;
    FLines: array of TLineData;
    FLineCount: Integer;
  public
    constructor Create( PixIn: PLPix );
    destructor Destroy; override;
    function RecognizeRect( inRect: TRect ): integer;
    property Text: string read FText;
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
  if FTesseract=nil then writeln('tesseract_new failed :(');
  tesseract_SetImage(FTesseract, FPageImage);
  FLineCount := 0;
  SetLength(FLines, FLineCount);
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
  UTF8Text: PChar;
  b: PLBox;
  wb: PLBox;
  boxCount: LongInt;
  lineNum: Longint;
  wbx, wby, wbw, wbh: Longint;
  LineData: TLineData;
  WordData: TWordData;

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
         R.Left := 0;
         R.Top := inRect.Bottom-inRect.Top;
         R.Right := 0;
         R.Bottom := 0;
         while (numaGetIValue(na, n, @lineNum)=0) and (lineNum=lineindex) do
               begin
                 wb := boxaGetBox(ba, n, L_CLONE);
                 boxGetGeometry(wb, @wbx, @wby, @wbw, @wbh);
                 if wby < R.Top then R.Top := wby;
                 if wbh + wby > R.Bottom then R.Bottom := wbh + wby;
                 if wbx + wbw > R.Right then R.Right := wbx + wbw;
                 boxDestroy(@wb);
                 Inc(n);
               end;
         ra[lineindex] := R;
         lineindex := lineNum;
         writeln( 'Lineindex: ', FormatFloat('0.00', lineindex));
       end;

     SetLength(FLines, FLineCount + linecount);
     writeln('Linecount: ', linecount);
     writeln('FLineCount: ', FLineCount);

      // --------- OCR inRect one line at a time to allow for progress callback --------
     for x := 0 to linecount-1 do
          begin
            wa := nil;
            R := ra[x];
            FLines[FLineCount + x].Index := x;
            FLines[FLineCount + x].Box := Bounds(inRect.Left + R.Left,
                                                 inRect.Top + R.Top,
                                                 R.Right-R.Left,
                                                 R.Bottom-R.Top);
            writeln(Format('ra(%d): %d %d %d %d', [x, R.Left, R.Top, R.Right, R.Bottom]));
            if RectIsValid(R) then
              begin
              writeln ('ocr line ', x);
              writeln(Format('line Rect: %d %d %d %d', [R.Left, R.Top, R.Right, R.Bottom]));
              tesseract_SetRectangle(FTesseract, inRect.Left + R.Left,
                                                 inRect.Top + R.Top,
                                                 R.Right-R.Left,
                                                 R.Bottom-R.Top);
              try
                UTF8Text := tesseract_GetUTF8Text(FTesseract);
                FText := Ftext + UTF8Text;
                tesseract_DeleteString(UTF8Text);
                except
                writeln('error in GetUTF8Text');
              end;
              n := length(Ftext);
              if FText[n]=#10 then SetLength(FText, n-1);
              wa := tesseract_GetWords(FTesseract, nil);
              SetLength(FLines[FLineCount + x].Words, boxaGetCount(wa));
              if wa<>nil then
                  begin
                     writeln('Tesseract wordcount: ', boxaGetCount(wa));
                     conf := tesseract_AllWordConfidences(FTesseract);
                    //writeln( boxaWrite(Pchar('/tmp/tessboxes' + IntToStr(x) + '.txt'), wa));
                     for n := 0 to boxaGetCount(wa)-1 do
                          begin
                            writeln('confidence ', conf[n]);
                            b := boxaGetBox(wa, n, L_COPY);
                            if b<>nil then
                            begin
                              AdjustToArea(b, x);
                              boxaReplaceBox(wa, n, b);
                            end;
                            FLines[FLineCount + x].Words[n].Confidence := conf[n];
                            FLines[FLineCount + x].Words[n].Box := BoxToRect(b);
                          end;
                     //writeln( boxaWrite(Pchar('/tmp/adjusted-tessboxes' + IntToStr(x) + '.txt'), wa));

                     tesseract_DeleteWordConfidences(conf);
                     boxaDestroy(@wa);
                  end;
              if Assigned(FOnOCRLine)
                 then FOnOCRLine(1);
            end
          else writeln('empty box: ', x);
         end;
    FLineCount := FLineCount + linecount;
    boxDestroy(@ba);
    ptaDestroy(@pa);
    numaDestroy(@na);
    pixDestroy(@TextRegion);

    writeln('FConfidenceRating sample: ', FLines[2].Words[0].Confidence);
end;

end.

