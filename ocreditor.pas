unit ocreditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Controls, StdCtrls, Forms, SysUtils, Graphics, ocr, SynMemo, SynEdit, SynHighlighterPosition,
  SynEditHighlighter, PasDoc_Aspell, frmSpell;


type
  TSpellcheckCallback = function( var aword: string; suggestions: TObjectVector ): TSpellResponse of object;
  TSelectTokenCallback = procedure( aline, aword: integer ) of object;

type

  { TOcrivistEdit }

  TOcrivistEdit = class( TSynMemo )
  private
    FOCRData: TTesseractPage;
    FOnSpellcheck: TSpellcheckCallback;
    FOnChangeToken: TSelectTokenCallback;
    FCurrentToken: TPoint;
    FStartToken: TPoint;
    TextNormal: TtkTokenKind;
    TextHighlight: TtkTokenKind;
    function GetHasSpellCheck: Boolean;
    function GetText: string;
    procedure SetOCRData ( const AValue: TTesseractPage ) ;
    function GetUpdatedToken( aline, aword: Integer ): string;
    procedure SetText;
  protected
    procedure KeyDown ( var Key: Word; Shift: TShiftState ) ; override;
    procedure KeyUp ( var Key: Word; Shift: TShiftState ) ; override;
    procedure MouseDown ( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
      override;
    procedure MouseUp ( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
      override;
    procedure SetCurrentToken( lline, charpos: Integer );
    function CorrectSpelling( var aword: string; suggestions: TObjectVector ): TSpellResponse;
    procedure LineRefresh( aline: Integer );
  public
    constructor Create ( AOwner: TComponent ) ; override;
    destructor Destroy; override;
    procedure DeleteLine ( lineindex: Integer ) ;
    procedure DeleteCurrentLine;
    procedure MergeLeft;
    procedure MergeRight;
    procedure Spellcheck;
    procedure HighlightToken( aline, aword: Integer );
    property OCRData: TTesseractPage read FOCRData write SetOCRData;
    property Text: string read GetText;
    property OnSpellCheck: TSpellcheckCallback read FOnSpellcheck write FOnSpellcheck;
    property OnSelectToken: TSelectTokenCallback read FOnChangeToken write FOnChangeToken;
    property HasSpellCheck: Boolean read GetHasSpellCheck;
  end;

implementation


{ TOcrivistEdit }

procedure TOcrivistEdit.SetOCRData ( const AValue: TTesseractPage ) ;
begin
  if FOCRData = AValue then exit;
  FOCRData := AValue;
  if FOCRData<>nil
      then SetText
      else Lines.Clear;
end;

function TOcrivistEdit.GetText: string;
var
  T: String;
  x: Integer;
begin
  T := '';
  for x := 0 to Lines.Count-1 do
      T := T + Lines[x];
  Result := T;
end;

function TOcrivistEdit.GetHasSpellCheck: Boolean;
begin
  Result := PathToAspell<>'';
end;

function TOcrivistEdit.GetUpdatedToken ( aline, aword: Integer ): string ;
var
  x: Integer;
  endpos: Integer;
  startpos: Integer;
begin
  endpos := Length( Lines[aline] );
  startpos := 1;
  for x := FOCRData.Lines[aline].WordCount-1 downto aword+1 do
      endpos := endpos-Length(FOCRData.Lines[aline].Words[x].Text+#32);
  for x := 0 to aword-1 do
      startpos := startpos+Length(FOCRData.Lines[aline].Words[x].Text+#32);
  Result := Copy(Lines[aline], startpos, endpos-startpos+1);
  {$IFDEF DEBUG}
  writeln( 'GetUpdatedToken=', Result);
  {$ENDIF}
end;

procedure TOcrivistEdit.SetText;
var
  x: Integer;
  c: Integer;
  L: String;
begin
  if FOCRData=nil then exit;
  Lines.Clear;
  for x := 0 to FOCRData.Linecount-1 do
     begin
       L := '';
       for c := 0 to FOCRData.Lines[x].WordCount-1 do
         L := L + FOCRData.Lines[x].Words[c].Text+#32;
       Lines.Add(L);
     end;
end;

procedure TOcrivistEdit.KeyDown ( var Key: Word; Shift: TShiftState ) ;
begin
  {$IFDEF DEBUG}
  WriteLn('TOcrivistEdit.KeyDown ', Key);
  {$ENDIF}
  if Key=8 then
     begin if Text[SelStart-1] in [#32, #10] then Key := 0; end   //TODO: change this later to permit token merging
  else if Key=46 then
     begin if Text[SelStart] in [#32, #10] then Key := 0; end     //TODO: change this later to permit token merging
  else if Key=32 then Key := 0;                                   //TODO: change this later to permit token insertion
 inherited KeyDown ( Key, Shift ) ;
end;

procedure TOcrivistEdit.KeyUp ( var Key: Word; Shift: TShiftState ) ;
begin
  inherited KeyUp ( Key, Shift ) ;
  if FOCRData=nil then Exit;
  with FOCRData.Lines[FCurrentToken.Y] do
     Words[FCurrentToken.X].Text := GetUpdatedToken(FCurrentToken.Y, FCurrentToken.X);
  {$IFDEF DEBUG} writeln(FOCRData.Lines[FCurrentToken.Y].Words[FCurrentToken.X].Text); {$ENDIF}
  if CaretY<>FCurrentToken.Y+1 then SetCurrentToken(CaretY-1, CaretX);
end;

procedure TOcrivistEdit.MouseDown ( Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer ) ;
begin
  inherited MouseDown ( Button, Shift, X, Y ) ;
  {$IFDEF DEBUG}
  writeln('TOcrivistEdit.MouseDown');
  writeln(x, #32, CaretX );
  writeln(Y, #32, CaretY );
  {$ENDIF}
  SetCurrentToken(CaretY-1, CaretX);
end;

procedure TOcrivistEdit.MouseUp ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  inherited MouseUp ( Button, Shift, X, Y ) ;
  {$IFDEF DEBUG}
  writeln('TOcrivistEdit.MouseUp');
  writeln(x, #32, CaretX );
  writeln(Y, #32, CaretY );
{$ENDIF}
  FStartToken := FCurrentToken;
  SetCurrentToken(CaretY-1, CaretX);
end;

procedure TOcrivistEdit.SetCurrentToken ( lline, charpos: Integer ) ;
var
  chars: Integer;
  w: Integer;
begin
  chars := 0;
  w := -1;
  if charpos>Length(Lines[lline]) then Exit;
  with FOCRData.Lines[lline] do
       begin
         while chars<charpos do
             begin
               Inc(w);
               chars := chars + Length(Words[w].Text)+1;
             end;
       end;
  FCurrentToken.Y := lline;
  FCurrentToken.X := w;
  if Assigned(FOnChangeToken)
       then FOnChangeToken( lline, w );
  {$IFDEF DEBUG} writeln('Token= ', FOCRData.Lines[lline].Words[w].Text);    {$ENDIF}
  //TSynPositionHighlighter(Highlighter).AddToken(lline, 3,TextHighlight);
end;

function TOcrivistEdit.CorrectSpelling ( var aword: string;
  suggestions: TObjectVector ) : TSpellResponse;
var
  x: Integer;
  S: String;
  p: SizeInt;
begin
  Result := srIgnore;
  with SpellcheckForm do
       begin
         SuggestionList.Clear;
         WordEdit.Text := aword;
         S := TSpellingError(suggestions.Items[0]).Suggestions;
         p := Pos(',', S);
         while p>0 do
             begin
               SuggestionList.Items.Add(Copy(S, 1, p-1));
               Delete(S, 1, p+1);
               p := Pos(',', S);
             end;
         if Length(S)>0 then SuggestionList.Items.Add(S);
         if ShowModal=mrOK
             then begin
                    aword := WordEdit.Text;
                    Result := SpellAction
                  end
             else Result := srCancel;
       end;
end;

procedure TOcrivistEdit.LineRefresh ( aline: Integer ) ;
var
  newline: String;
  x: Integer;
begin
  newline := '';
  for x := 0 to FOCRData.Lines[aline].WordCount-1 do
          newline := newline + FOCRData.Lines[aline].Words[x].Text + #32;
  Lines[aline] := newline;
end;

constructor TOcrivistEdit.Create ( AOwner: TComponent ) ;
var
  PosHighlighter: TSynPositionHighlighter;
begin
  inherited Create ( AOwner ) ;
  // create highlighter
  PosHighlighter:=TSynPositionHighlighter.Create(Self);

  // add some attributes
  TextNormal := PosHighlighter.CreateTokenID('Normal', Font.Color,clNone,[]);
  TextHighlight := PosHighlighter.CreateTokenID('Highlight',clRed,clYellow,[fsBold]);

  Highlighter := PosHighlighter;

  //HideSelection := true;
  Options := Options-[eoAutoIndent, eoGroupUndo, eoScrollPastEol, eoSmartTabs]
                    +[{eoNoSelection, }eoHideRightMargin];
  ScrollBars := ssAutoBoth;
  FOnSpellcheck := @CorrectSpelling;
  //Keystrokes.Clear;
  //MouseActions.Clear;
  //MouseSelActions.Clear;
end;

destructor TOcrivistEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TOcrivistEdit.DeleteLine ( lineindex: Integer ) ;
var
  x: LongInt;
begin
  if (lineindex<0) or (lineindex>=FOCRData.Linecount)
      then raise Exception.Create('Out of Range in TOcrivistEdit.DeleteLine: ' + IntToStr(lineindex));
  for x := lineindex to FOCRData.Linecount-2 do
      FOCRData.Lines[x] := FOCRData.Lines[x+1];
  FOCRData.Linecount := FOCRData.Linecount-1;
  Lines.Delete(lineindex);
end;

procedure TOcrivistEdit.DeleteCurrentLine;
begin
  DeleteLine(FCurrentToken.Y);
end;

procedure TOcrivistEdit.MergeLeft;
// Combine current token with the token on its left
var
  lineindex: LongInt;
  wordindex: LongInt;
begin
  wordindex := FCurrentToken.X;
  if wordindex>0 then
     begin
       FCurrentToken.X := FCurrentToken.X-1;
       MergeRight;
     end;
end;

procedure TOcrivistEdit.MergeRight;
// Combine current token with the token on its right
var
  lineindex: LongInt;
  wordindex: LongInt;
  CurrentToken: TWordData;
  NextToken: TWordData;
  x: Integer;
begin
  lineindex := FCurrentToken.Y;
  wordindex := FCurrentToken.X;
  if wordindex+1 < OCRData.Lines[lineindex].WordCount then
     begin
       CurrentToken := OCRData.Lines[lineindex].Words[wordindex];
       NextToken := OCRData.Lines[lineindex].Words[wordindex+1];
       CurrentToken.Box.Right := NextToken.Box.Right;
       CurrentToken.Length := CurrentToken.Length + NextToken.Length;
       CurrentToken.Text := CurrentToken.Text + NextToken.Text;
       with OCRData.Lines[lineindex] do
          begin
            Words[wordindex] := CurrentToken;
            for x := wordindex+1 to WordCount-2 do
             Words[x] := Words[x+1];
            WordCount := WordCount-1;
          end;
       if Assigned(FOnChangeToken)
           then FOnChangeToken( lineindex, wordindex ); // refresh original text view
     end;
end;

procedure TOcrivistEdit.Spellcheck;
var
  i, j: Integer;
  suggestions: TObjectVector;
  Speller: TAspellProcess;
  wword: Integer;
  lline: Integer;
  w: String;
  spellcheckresponse: TSpellResponse;

  function TrimPunctuation( aword: string ): string;  // very crude and only reliable for English - feel free to improve this :)
  var
    p: Integer;
    wordin: String;
  begin
     Result := '';
     wordin := aword;
     if Length(wordin)=0 then Exit;
     p := 1;
     if p<length(wordin) then
        while (not (wordin[p] in ['a'..'z'] + ['A'..'Z'] + ['0'..'9'] + [#32] + ['-']))
              and (p<length(wordin))
              do Inc(p);
     Delete(wordin, 1, p-1);
     wordin := #32 + wordin;
     p := Length(wordin);
     if p>0 then
        while (not (wordin[p] in ['a'..'z'] + ['A'..'Z'] + ['0'..'9'] + [#32] + ['-']))
              and (p>0)
              do Dec(p);
     if p < Length(wordin)
          then Delete(wordin, p+1, MaxInt);
     Result := Trim(wordin);
  end;

begin
  if FOCRData=nil then Exit;
  try
  suggestions := TObjectVector.Create(false);
    try
    Speller := TAspellProcess.Create('', 'en', nil);
    for lline := 0 to FOCRData.Linecount-1 do
       with FOCRData.Lines[lline]do
            for wword := 0 to WordCount-1 do
               begin
                 {$IFDEF DEBUG} writeln(Words[wword].Text, ': l w ', lline, #32, wword); {$ENDIF}
                 w := TrimPunctuation( Words[wword].Text );
                 if length(w)>0 then
                    if (w[Length(w)]='-') and (wword=WordCount-1) then w:= '';  //TODO: deal with hyphens instead of skipping them
                 if Length(w)>0 then
                    Speller.CheckString(w, suggestions); // spellcheck each word
                 if suggestions.Count>0 then
                    begin
                      HighlightToken(lline, wword);
                      if Assigned(FOnChangeToken)
                           then FOnChangeToken( lline, wword );
                      spellcheckresponse := CorrectSpelling(w, suggestions);
                      TSynPositionHighlighter(Highlighter).ClearTokens(lline);
                      if spellcheckresponse=srCancel then Exit else
                      case spellcheckresponse of
                           srChange: begin
                                    Words[wword].Text := w;
                                    LineRefresh(lline);
                                  end;
                           srAdd: begin
                                    Words[wword].Text := w;
                                    LineRefresh(lline);
                                    Speller.AddToPersonalDict(w);
                                  end;
                           srIgnore: begin
                                    LineRefresh(lline);
                                    Speller.AcceptForSession(w);
                                  end;
                           end;
                      Refresh;
                    end;
               end;
    except


    end;
  finally
    Speller.SaveWordlist;
    suggestions.Free;
    Speller.Free;
  end;
end;

procedure TOcrivistEdit.HighlightToken ( aline, aword: Integer ) ;
var
  charoffset: Integer;
  x: Integer;
begin
  charoffset := 0;
  for x := 0 to aword-1 do
     charoffset := charoffset + Length(FOCRData.Lines[aline].Words[x].Text + #32);
  TSynPositionHighlighter(Highlighter).AddToken(aline,
          charoffset + Length(FOCRData.Lines[aline].Words[aword].Text), TextHighlight);
  TSynPositionHighlighter(Highlighter).AddToken(aline, charoffset, TextNormal);
  if aline>5 then TopLine := aline-3;
  Refresh;
end;

end.

