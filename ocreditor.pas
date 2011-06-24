unit ocreditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Controls, StdCtrls, Forms, SysUtils, Graphics, ocr, SynMemo, SynEdit, SynHighlighterPosition,
  SynEditHighlighter, SynHighlighterAny;

type

  { TOCREditor }

  TOCREditor = class( TScrollingWinControl )
  private
    FOCRData: TTesseractPage;
    FCanvasPanel: TPanel;
    AttrNormal: TtkTokenKind;
    AttrOCRWord: TtkTokenKind;
    procedure SetOCRData ( const AValue: TTesseractPage ) ;
    public
      procedure Paint; override;
      constructor Create ( AOwner: TComponent ) ; override;
      destructor Destroy; override;
      property OCRData: TTesseractPage read FOCRData write SetOCRData;
  end;

  { TOcrivistEdit }

  TOcrivistEdit = class( TSynMemo )
  private
    FOCRData: TTesseractPage;
    FCurrentToken: TPoint;
    function GetText: string;
    procedure SetOCRData ( const AValue: TTesseractPage ) ;
    function GetUpdatedToken( aline, aword: Integer ): string;
    procedure SetText;
  protected
    procedure KeyDown ( var Key: Word; Shift: TShiftState ) ; override;
    procedure KeyUp ( var Key: Word; Shift: TShiftState ) ; override;
    procedure MouseDown ( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
      override;
    procedure SetCurrentToken( lline, charpos: Integer );
  public
    constructor Create ( AOwner: TComponent ) ; override;
    destructor Destroy; override;
    property OCRData: TTesseractPage read FOCRData write SetOCRData;
    property Text: string read GetText;
  end;

implementation

{ TOCREditor }

procedure TOCREditor.SetOCRData ( const AValue: TTesseractPage ) ;
begin
  if FOCRData = AValue then exit;
  writeln('TOCREDitor.SetOCRData');
  FOCRData := AValue;
end;

procedure TOCREditor.Paint;
var
  lline: Integer;
  w: Integer;
  offsetX: Integer;
  offsetY: LongInt;
  leftmargin: Integer;
  topmargin: Integer;
begin
  Canvas.Brush.Color := clWindow;
  canvas.Rectangle(ClientRect);
  offsetY := Trunc(Canvas.TextHeight('Yy')*1.3);
  topmargin := 5;
  leftmargin := Canvas.TextWidth(' ');
  if Assigned(FOCRData) then
     for lline := 0 to FOCRData.Linecount-1 do
       begin
         offsetX := 0;
         for w := 0 to FOCRData.Lines[lline].WordCount-1 do
           with FCanvasPanel do
             begin
               if FOCRData.Lines[lline].Words[w].Confidence>75
                   then Canvas.Font.Color := clBlack
                   else Canvas.Font.Color := clRed;
               Canvas.TextOut(leftmargin + offsetX, topmargin + (offsetY*lline), FOCRData.Lines[lline].Words[w].Text);
               offsetX := offsetX + Canvas.TextWidth(FOCRData.Lines[lline].Words[w].Text) + leftmargin;
               if offsetX>Width then Width := offsetX ;
             end;
       end;
  inherited Paint;
end;

constructor TOCREditor.Create ( AOwner: TComponent ) ;
begin
  inherited Create ( AOwner ) ;
  FCanvasPanel := TPanel.Create(self);
  FCanvasPanel.Parent := Self;
  FCanvasPanel.Height := 200;
  SetAutoScroll(true);
  FOCRData := nil;
end;

destructor TOCREditor.Destroy;
begin
  inherited Destroy;
end;

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
  writeln( 'GetUpdatedToken=', Result);
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
  WriteLn('TOcrivistEdit.KeyDown ', Key);
  if Key=8 then
     if Text[SelStart-1] in [#32, #10] then Key := 0   //TODO: change this later to permit token merging
  else if Key=46 then
     if Text[SelStart] in [#32, #10] then Key := 0   //TODO: change this later to permit token merging
  else if Key=32 then Key := 0;
 inherited KeyDown ( Key, Shift ) ;
end;

procedure TOcrivistEdit.KeyUp ( var Key: Word; Shift: TShiftState ) ;
begin
  inherited KeyUp ( Key, Shift ) ;
  with FOCRData.Lines[FCurrentToken.Y] do
     Words[FCurrentToken.X].Text := GetUpdatedToken(FCurrentToken.Y, FCurrentToken.X);
  writeln(FOCRData.Lines[FCurrentToken.Y].Words[FCurrentToken.X].Text);
  if CaretY<>FCurrentToken.Y+1 then SetCurrentToken(CaretY-1, CaretX);
end;

procedure TOcrivistEdit.MouseDown ( Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer ) ;
begin
  inherited MouseDown ( Button, Shift, X, Y ) ;
  writeln(x, #32, CaretX );
  writeln(Y, #32, CaretY );
  SetCurrentToken(CaretY-1, CaretX);
end;

procedure TOcrivistEdit.SetCurrentToken ( lline, charpos: Integer ) ;
var
  chars: Integer;
  w: Integer;
begin
  chars := 0;
  w := -1;
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
  writeln('Token= ', FOCRData.Lines[lline].Words[w].Text);
end;

constructor TOcrivistEdit.Create ( AOwner: TComponent ) ;
begin
 { // create highlighter
  Highlighter:=TSynPositionHighlighter.Create(Self);

  // add some attributes
  Attr1:=Highlighter.CreateTokenID('Attr1',clRed,clNone,[]);
  Attr2:=Highlighter.CreateTokenID('Attr2',clBlue,clNone,[fsBold]); }

  inherited Create ( AOwner ) ;
  HideSelection := true;
  Options := Options-[eoAutoIndent, eoGroupUndo, eoScrollPastEol, eoSmartTabs]
                    +[eoNoSelection, eoHideRightMargin];
  ScrollBars := ssAutoBoth;
  //Keystrokes.Clear;
  //MouseActions.Clear;
  //MouseSelActions.Clear;
end;

destructor TOcrivistEdit.Destroy;
begin
  inherited Destroy;
end;

end.

