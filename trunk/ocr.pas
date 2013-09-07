unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, LibLeptUtils, tesseract;

const
  BIN_THRESHOLD = 150;

  LanguageTokens: array [0..32] of string = ('chi_tra',
                                            'chi_sim',
                                            'ind',
                                            'swe',
                                            'ron',
                                            'slv',
                                            'srp',
                                            'tgl',
                                            'tur',
                                            'hun',
                                            'fin',
                                            'ita',
                                            'nld',
                                            'nor',
                                            'jpn',
                                            'vie',
                                            'spa',
                                            'ukr',
                                            'fra',
                                            'slk',
                                            'kor',
                                            'ell',
                                            'rus',
                                            'por',
                                            'bul',
                                            'lav',
                                            'lit',
                                            'pol',
                                            'deu',
                                            'dan',
                                            'ces',
                                            'cat',
                                            'eng');

  Languages: array [0..32] of string = ('Chinese (Traditional)',
                                        'Chinese (Simplified)',
                                        'Indonesian',
                                        'Swedish',
                                        'Romanian',
                                        'Slovene',
                                        'Serbian (Latin)',
                                        'Tagalog',
                                        'Turkish',
                                        'Hungarian',
                                        'Finnish',
                                        'Italian',
                                        'Dutch',
                                        'Norwegian',
                                        'Japanese',
                                        'Vietnamese',
                                        'Spanish',
                                        'Ukrainian',
                                        'French',
                                        'Slovak',
                                        'Korean',
                                        'Greek',
                                        'Russian',
                                        'Portuguese',
                                        'Bulgarian',
                                        'Latvian',
                                        'Lithuanian',
                                        'Polish',
                                        'German',
                                        'Danish',
                                        'Czech',
                                        'Catalan',
                                        'English');

type
   TConfidenceList = array of PInteger;

   TWordData = record
     Text: string;
     Start: Integer;
     Length: Integer;
     Box: TRect;
     Confidence: Byte;
   end;

   TLineData = record
     Index: Integer;
     Box: TRect;
     WordCount: Integer;
     Words: Array of TWordData;
   end;

  { TTesseractPage }

  TTesseractPage = class(TObject)
  private
    FDatapath: string;
    FLanguage: string;
    FOnOCRLine: TProgressCallback;
    FPageImage: PLPix;
    FTesseract: TessAPIHandle;
    FBoxes: PBoxArrayArray;
    FConfidenceRating: TConfidenceList;
    FText: String;
    FLines: array of TLineData;
    FLineCount: Integer;
    function GetLines ( lineIndex: Integer ) : TLineData;
    procedure SetLinecount ( const AValue: Integer ) ;
    procedure SetLines ( lineIndex: Integer ; const AValue: TLineData ) ;
  public
    constructor Create( PixIn: PLPix; data, lang: PChar );
    destructor Destroy; override;
    function RecognizeRect( inRect: TRect ): integer;
    property Text: string read FText write FText;
    property OnOCRLine: TProgressCallback read FOnOCRLine write FOnOCRLine;
    property Lines[ lineIndex: Integer ]: TLineData read GetLines write SetLines;
    property Linecount: Integer read FLineCount write SetLinecount;
    property Datapath: string read FDatapath;
    property Language: string read FLanguage;
  end;

  function GetLanguageFromToken(langtoken: string): string;
  function GetLanguageToken(lang: string): string;

implementation

function GetLanguageFromToken(langtoken: string): string;
var
  x: Integer;
begin
  x := 0;
  Result := '';
  while (x < Length(LanguageTokens)) and (Result ='') do
        begin
          if LanguageTokens[x]=langtoken
             then Result := Languages[x];
          Inc(x);
        end;
end;

function GetLanguageToken(lang: string): string;
var
  x: Integer;
begin
  x := 0;
  Result := '';
  while (x < Length(Languages)) and (Result ='') do
        begin
          if Languages[x]=lang
             then Result := LanguageTokens[x];
          Inc(x);
        end;
end;

{ TTesseractPage }

function TTesseractPage.GetLines ( lineIndex: Integer ) : TLineData;
begin
  if (lineIndex>=0) and (lineIndex<FLineCount)
     then Result := FLines[lineIndex];
end;

procedure TTesseractPage.SetLinecount ( const AValue: Integer ) ;
begin
  SetLength(FLines, AValue);
  FLineCount := AValue;
end;

procedure TTesseractPage.SetLines ( lineIndex: Integer ;
  const AValue: TLineData ) ;
begin
  if lineIndex>=FLineCount then exit;
  FLines[lineIndex] := AValue;
end;

constructor TTesseractPage.Create ( PixIn: PLPix; data, lang: Pchar ) ;
var
  d: LongInt;
 begin
  d := pixGetDepth(PixIn);
  FDatapath := data;
  FLanguage := lang;
  if d = 32
     then FPageImage := pixGenerateMaskByBand32(PixIn, BIN_THRESHOLD, BIN_THRESHOLD, BIN_THRESHOLD)
  else if d >=4
     then FPageImage := pixThresholdToBinary(PixIn, BIN_THRESHOLD)
  else FPageImage := pixClone(PixIn);
//  pixWrite('/tmp/testOCRimage.bmp', FPageImage, IFF_BMP);
  if FileExists(FDatapath + FLanguage + '.traineddata') then
  try
    FTesseract := TessBaseAPICreate;
    TessBaseAPIInit3 (FTesseract, nil, nil);
    TessBaseAPISetImage2(FTesseract, FPageImage);
    FLineCount := 0;
    SetLength(FLines, FLineCount);
  except
    if FTesseract=nil then
       writeln('tesseract_new failed :(')
  end
  else writeln('Unable to open ' + FDatapath + FLanguage + '.traineddata') ; //TODO: handle this better!

end;

destructor TTesseractPage.Destroy;
begin
  if FPageImage<>nil then pixDestroy(@FPageImage);
  TessBaseAPIDelete(FTesseract);
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
  lncount: Integer;
  ra : array[0..255] of TRect;
  n: Integer;
  R: TRect;
  TextRegion: PLPix;
  conf: PIntegerArray;
  UTF8Text: PChar;
  b: PLBox;
  wb: PLBox;
  boxCount: LongInt;
  lineNum: Longint;
  wbx, wby, wbw, wbh: Longint;
  LineData: TLineData;
  WordData: TWordData;
  wordpos: Integer;
  wordend: Integer;
  deletedtokens: Integer;
// debug variables
  pixa: PPixArray;
  pixd: PLPix;
  cmap: Pointer;

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
    pa := nil;
    na := nil;
    TextRegion := CropPix(FPageImage, inRect);
    writeln('Created TextRegion');
    pixWrite('/tmp/textregion.bmp', TextRegion, IFF_BMP);
    writeln(Format('inRect: %d %d %d %d', [inrect.Left, inRect.Top, inRect.Right, inRect.Bottom]));
    writeln( pixGetWordsInTextlines(TextRegion, 1, 5, 20, inRect.Right-inRect.Left, 200, @ba, @pa, @na) );
    writeln('Boxarray.Getcount: ', boxaGetCount(ba));
    lncount := 0;

     // --------- loop through word boxes to construct line boxes --------
    n := 0;
    boxCount := boxaGetCount(ba);
    lineindex := 0;
    lncount := 0;
    if boxCount>0 then
        while numaGetIValue(na, n, @linenum)=0 do
           begin
             Inc(lncount);
             R.Left := 0;
             R.Top := inRect.Bottom-inRect.Top;
             R.Right := inRect.Right-inRect.Left;
             R.Bottom := 0;
             wb := nil;
             while (numaGetIValue(na, n, @lineNum)=0) and (lineNum=lineindex) do
                   begin
                     wb := boxaGetBox(ba, n, L_CLONE);
                     boxGetGeometry(wb, @wbx, @wby, @wbw, @wbh);
                     if wby < R.Top then R.Top := wby;
                     if wbh + wby > R.Bottom then R.Bottom := wbh + wby;
                     boxDestroy(@wb);
                     Inc(n);
                   end;
             ra[lineindex] := R;
             lineindex := lineNum;
             writeln( 'Lineindex: ', FormatFloat('0.00', lineindex));
           end;

     SetLength(FLines, FLineCount + lncount);
     writeln('Lncount: ', lncount);
     writeln('FLineCount: ', FLineCount);

      // --------- OCR inRect one line at a time to allow for progress callback --------
     for x := 0 to lncount-1 do
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
              TessBaseAPISetRectangle(FTesseract, inRect.Left + R.Left,
                                                 inRect.Top + R.Top,
                                                 R.Right-R.Left,
                                                 R.Bottom-R.Top);
              try
                UTF8Text := TessBaseAPIGetUTF8Text(FTesseract);
                FText := Ftext + UTF8Text;
                except
                writeln('error in GetUTF8Text');
              end;
              n := length(Ftext);
              if FText[n]=#10 then SetLength(FText, n-1);
              wa := TessBaseAPIGetWords(FTesseract, @pixa);

              {  For debugging:
              // Display each component as a random color in cmapped 8 bpp.
              // Background is color 0; it is set to white. */
          pixd := pixaDisplayRandomCmap(pixa, pixGetWidth(TextRegion), pixGetHeight(TextRegion));
          cmap := pixGetColormap(pixd);
          pixcmapResetColor(cmap, 0, 255, 255, 255);  // reset background to white */
          pixDisplayWrite(pixd, 1);     }


              SetLength(FLines[FLineCount + x].Words, boxaGetCount(wa));
              FLines[FLineCount + x].Wordcount := boxaGetCount(wa);
              wordpos := 1;
              wordend := 1;
              if wa<>nil then
                  begin
                     writeln('Tesseract wordcount: ', boxaGetCount(wa));
                     deletedtokens := 0;
                     conf := TessBaseAPIAllWordConfidences(FTesseract);
                    //writeln( boxaWrite(Pchar('/tmp/tessboxes' + IntToStr(x) + '.txt'), wa));
                     for n := 0 to boxaGetCount(wa)-1 do
                          begin
                            writeln('confidence ', conf^[n]);
                            b := boxaGetBox(wa, n, L_COPY);
                            if b<>nil then
                            begin
                              AdjustToArea(b, x);
                              boxaReplaceBox(wa, n, b);
                            end;
                            FLines[FLineCount + x].Words[n-deletedtokens].Confidence := conf^[n];
                            FLines[FLineCount + x].Words[n-deletedtokens].Box := BoxToRect(b);
                            while UTF8Text[wordend]>#32 do Inc(wordend);
                            Inc(wordend);
                            FLines[FLineCount + x].Words[n-deletedtokens].Start := wordpos;
                            FLines[FLineCount + x].Words[n-deletedtokens].Length := wordend-wordpos;
                            FLines[FLineCount + x].Words[n-deletedtokens].Text := Copy(UTF8Text, wordpos, wordend-wordpos);
                            while UTF8Text[wordend]<=#32 do Inc(wordend);
                            wordpos := wordend+1;
                            if Length(Trim(UTF8Text))=0 then
                               begin
                                 Inc (deletedtokens);
                                 FLines[FLineCount + x].WordCount := FLines[FLineCount + x].WordCount-1;
                               end;
                          end;
                     //writeln( boxaWrite(Pchar('/tmp/adjusted-tessboxes' + IntToStr(x) + '.txt'), wa));

                     TessDeleteIntArray(conf);
                     boxaDestroy(@wa);
                  end;
              TessDeleteText(UTF8Text);
              if Assigned(FOnOCRLine)
                 then FOnOCRLine(1);
            end
          else writeln('empty box: ', x);
          for n := 0 to FLines[FLineCount + x].WordCount-1 do write('[',n, #32, FLines[FLineCount + x].Words[n].Text+'] ');
          WriteLn('');
         end;

    FLineCount := FLineCount + lncount;
    if ba<>nil then boxDestroy(@ba);
    if pa<>nil then ptaDestroy(@pa);
    if na<>nil then numaDestroy(@na);
    if TextRegion<>nil then pixDestroy(@TextRegion);
end;

end.

