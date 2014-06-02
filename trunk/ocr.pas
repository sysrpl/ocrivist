unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, LibLeptUtils, tesseract;

type
  TLanguageCode = record
    AspellCode: string[8];
    TessCode: string[8];
    EngName: string;
    NativeName: string;
  end;

const
  BIN_THRESHOLD = 150;

  {LanguageData: array [0..2] of TLanguageCode =
                (('', 'chi_tra', 'Chinese (Traditional)', ''),
                 ['', 'chi_sim', 'Chinese (Simplified)', ''],
                 ['id', 'ind', 'Indonesian', 'Bahasa Indonesia']);}

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
   //TConfidenceList = array of PInteger;

   TFontAttrib = (faBold, faItalic, faSerif, faMonospace);
   TFontAttributes = set of TFontAttrib;

   TWordData = record
     Text: string;
     Start: Integer;
     Length: Integer;
     Box: TRect;
     Baseline:integer;
     Confidence: Byte;
     FontID: Integer;
     FontSize: Byte;
     FontFlags: TFontAttributes;
     isNumeric: Boolean;
   end;

   // TWordDataV3 is the record size used for file version =<3
   TWordDataV3 = record
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
    //FConfidenceRating: TConfidenceList;
    FText: String;      // Remove?
    FLines: array of TLineData;
    FLineCount: Integer;
    FFontList: TStringList;
    function GetLines ( lineIndex: Integer ) : TLineData;
    procedure SetLinecount ( const AValue: Integer ) ;
    procedure SetLines ( lineIndex: Integer ; const AValue: TLineData ) ;
  public
    constructor Create( PixIn: PLPix );
    destructor Destroy; override;
    function RecognizeRect( inRect: TRect ): integer;
    function RecognizeAll: integer;
    function Initialise( data, lang: PChar ): integer;
    procedure Close;
    property Text: string read FText write FText;
    property OnOCRLine: TProgressCallback read FOnOCRLine write FOnOCRLine;
    property Lines[ lineIndex: Integer ]: TLineData read GetLines write SetLines;
    property Linecount: Integer read FLineCount write SetLinecount;
    property Datapath: string read FDatapath;
    property Language: string read FLanguage;
    property Fonts: TStringList read FFontList write FFontList;
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

constructor TTesseractPage.Create ( PixIn: PLPix ) ;
var
  d: LongInt;
begin
  FPageImage := nil;
  if PixIn<>nil then
     begin
        d := pixGetDepth(PixIn);
        if d = 32
           then FPageImage := pixGenerateMaskByBand32(PixIn, BIN_THRESHOLD, BIN_THRESHOLD, BIN_THRESHOLD)
        else if d >=4
           then FPageImage := pixThresholdToBinary(PixIn, BIN_THRESHOLD)
        else FPageImage := pixClone(PixIn);
     end;

    FLineCount := 0;
    SetLength(FLines, FLineCount);
    FFontList := TStringList.Create;
end;

destructor TTesseractPage.Destroy;
begin
  if FPageImage<>nil then pixDestroy(@FPageImage);
  Close;
  boxaaDestroy(@FBoxes);
  FFontList.Free;
  inherited Destroy;
end;

function TTesseractPage.RecognizeRect ( inRect: TRect ) : integer;
var
  pa: PPixArray;
  ba: PBoxArray;
  wa: PBoxArray;
  na: PNumArray;
  baseline: PPointArray;
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
  wordcount: Integer;
  ri: TessResultIterator;
  isBold, isItalic, isMonospace, isSerif, isSC, isUL: Boolean;
  pointsize, fontid: Integer;
  fontname: PChar;
  fi: Integer;

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
    ba := nil;
    pa := nil;
    na := nil;
    baseline := nil;
    TextRegion := CropPix(FPageImage, inRect);
    //pixWrite('/tmp/textregion.bmp', TextRegion, IFF_BMP);
    pixGetWordsInTextlines(TextRegion, 1, 5, 20, inRect.Right-inRect.Left, 200, @ba, @pa, @na);
    {$IFDEF DEBUG}
    writeln(Format('inRect: %d %d %d %d', [inrect.Left, inRect.Top, inRect.Right, inRect.Bottom]));
    writeln('Boxarray.Getcount: ', boxaGetCount(ba));
    {$ENDIF}
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
             {$IFDEF DEBUG}
             writeln( 'Lineindex: ', FormatFloat('0.00', lineindex));
             {$ENDIF}
           end;

     SetLength(FLines, FLineCount + lncount);
     {$IFDEF DEBUG}
     writeln('Lncount: ', lncount);
     writeln('FLineCount: ', FLineCount);
     {$ENDIF}
     pixFindBaselines(TextRegion, @baseline, 0);

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
            writeln(Format('baseline: %g', [baseline^.y[x*2]]));
            if RectIsValid(R) then
              begin
              {$IFDEF DEBUG}
              writeln ('ocr line ', x);
              writeln(Format('line Rect: %d %d %d %d', [R.Left, R.Top, R.Right, R.Bottom]));
              {$ENDIF}
              TessBaseAPISetRectangle(FTesseract, inRect.Left + R.Left,
                                                 inRect.Top + R.Top,
                                                 R.Right-R.Left,
                                                 R.Bottom-R.Top);
              try
                UTF8Text := TessBaseAPIGetUTF8Text(FTesseract);
                FText := Ftext + UTF8Text;
                except
                {$IFDEF DEBUG}
                writeln('error in GetUTF8Text');
                {$ENDIF}
              end;
              n := length(UTF8Text);
              if n>0 then
                 begin
                   wordpos := 1;
                   wordend := 1;
                   if FText[n]=#10 then SetLength(FText, n-1);
                   wa := TessBaseAPIGetWords(FTesseract, @pixa);

                   {  For debugging:
                   // Display each component as a random color in cmapped 8 bpp.
                   // Background is color 0; it is set to white. */
               pixd := pixaDisplayRandomCmap(pixa, pixGetWidth(TextRegion), pixGetHeight(TextRegion));
               cmap := pixGetColormap(pixd);
               pixcmapResetColor(cmap, 0, 255, 255, 255);  // reset background to white */
               pixDisplayWrite(pixd, 1);     }

                   wordcount := boxaGetCount(wa);
                   SetLength(FLines[FLineCount + x].Words, wordcount);
                   FLines[FLineCount + x].Wordcount := wordcount;
                   {$IFDEF DEBUG}
                   writeln('Tesseract wordcount: ', wordcount);
                   {$ENDIF}
                   ri := TessBaseAPIGetIterator(FTesseract);
                   n := 0;
                   deletedtokens := 0;
                   if ri<>nil then
                        repeat
                          fontname := TessResultIteratorWordFontAttributes(ri, @isBold, @isItalic, @isUL, @isMonospace,
                                      @isSerif, @isSC, @pointsize, @fontid);
                          fi := FFontList.IndexOf(fontname);
                          if fi<0 then
                             begin FFontList.Add(fontname); fi := FFontList.Count-1; end;
                          {$IFDEF DEBUG}
                     //     writeln(Format('line: %d %d %s %d id: %d ', [x, n, fontname, pointsize, fontid]));
                     //     WriteLn(Format( 'Confidence: %g', [TessResultIteratorConfidence(ri, RIL_WORD)]));
                          {$ENDIF}
                          b := boxaGetBox(wa, n, L_COPY);
                          if b<>nil then
                          begin
                            AdjustToArea(b, x);
                            boxaReplaceBox(wa, n, b);
                          end;
                          FLines[FLineCount + x].Words[n-deletedtokens].Box := BoxToRect(b);
                          FLines[FLineCount + x].Words[n-deletedtokens].Baseline := inRect.Top + trunc( baseline^.y[x*2] );
                          FLines[FLineCount + x].Words[n-deletedtokens].Confidence := Trunc( TessResultIteratorConfidence(ri, RIL_WORD) );
                          FLines[FLineCount + x].Words[n-deletedtokens].FontID := fi;
                          FLines[FLineCount + x].Words[n-deletedtokens].isNumeric := TessResultIteratorWordIsNumeric(ri);
                          FLines[FLineCount + x].Words[n-deletedtokens].FontSize := pointsize;
                          FLines[FLineCount + x].Words[n-deletedtokens].Text := TessResultIteratorGetUTF8Text(ri, RIL_WORD);
                          FLines[FLineCount + x].Words[n-deletedtokens].Start := wordpos;
                          FLines[FLineCount + x].Words[n-deletedtokens].Length := Length(FLines[FLineCount + x].Words[n-deletedtokens].Text);
                          if isBold then FLines[FLineCount + x].Words[n-deletedtokens].FontFlags := FLines[FLineCount + x].Words[n-deletedtokens].FontFlags + [faBold];
                          if isItalic then FLines[FLineCount + x].Words[n-deletedtokens].FontFlags := FLines[FLineCount + x].Words[n-deletedtokens].FontFlags + [faItalic];
                          if isSerif then FLines[FLineCount + x].Words[n-deletedtokens].FontFlags := FLines[FLineCount + x].Words[n-deletedtokens].FontFlags + [faSerif];
                          if isMonospace then FLines[FLineCount + x].Words[n-deletedtokens].FontFlags := FLines[FLineCount + x].Words[n-deletedtokens].FontFlags + [faMonospace];
                          wordend := wordend + FLines[FLineCount + x].Words[n-deletedtokens].Length;
                          wordpos := wordend + 1;
                          Inc(n);
                        until not TessPageIteratorNext(ri,RIL_WORD);
                   TessPageIteratorDelete(ri);
                 end;
              if wa<>nil
                 then boxaDestroy(@wa);
              TessDeleteText(UTF8Text);
              if Assigned(FOnOCRLine)
                 then FOnOCRLine(1);
            end
          else {$IFDEF DEBUG} writeln('empty box: ', x) {$ENDIF};
          {$IFDEF DEBUG}
          for n := 0 to FLines[FLineCount + x].WordCount-1 do write('[',n, #32, FLines[FLineCount + x].Words[n].Text+'] ');
          WriteLn('');
          {$ENDIF}
         end;

    FLineCount := FLineCount + lncount;
    if ba<>nil then boxaDestroy(@ba);
    if pa<>nil then ptaDestroy(@pa);
    if na<>nil then numaDestroy(@na);
    if baseline<>nil then ptaDestroy(@baseline);
    if TextRegion<>nil then pixDestroy(@TextRegion);
    for x := 0 to FFontList.Count-1 do
         WriteLn(FFontList.Strings[x]);
end;

function TTesseractPage.RecognizeAll: integer;
var
  R: TRect;
  w, h, d: Integer;
begin
  pixGetDimensions(FPageImage, @w, @h, @d);
  R.Left := 0;
  R.Top := 0;
  R.Right := w;
  R.Bottom := h;
  RecognizeRect(R);
end;

function TTesseractPage.Initialise ( data, lang: PChar ) : integer;
begin
  Result := -1;
  FDatapath := data;
  FLanguage := lang;
  try
    FTesseract := TessBaseAPICreate;
    TessBaseAPIInit3 (FTesseract, data, lang);
    if FPageImage<>nil
       then TessBaseAPISetImage2(FTesseract, FPageImage);
  except
    {$IFDEF DEBUG}
    if FTesseract=nil then
       writeln('tesseract_new failed :(')
    //writeln('Unable to open ' + FDatapath + FLanguage + '.traineddata') ; //TODO: handle this better!
    {$ENDIF}
  end;
  if FTesseract<>nil then Result := 0;
end;

procedure TTesseractPage.Close;
begin
  if FTesseract<>nil
    then TessBaseAPIDelete(FTesseract);
  FTesseract := nil;
end;

end.

