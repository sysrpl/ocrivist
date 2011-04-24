unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, leptonica, LibLeptUtils;

type

  TViewermode = (vmFitToWindow, vmFitToWidth, vmFitToHeight, vmScaled, vmFullSize);

  { TImageCanvas }

  TImageCanvas = class (TCustomControl)
  private
    { private declarations }
    FSelectRect: TRect;
    FSelecting: Boolean;
    FOnChangeBitmap: TNotifyEvent;
    FCurrentPagePix: TLeptPix;
    FPageWidth: Integer;
    FPageHeight: Integer;
    FBitmap: TBitmap;
    FClientRect: TRect;
    FMode: TViewermode;
    FScale: Single;
    FVertScrollBar: TScrollBar;
    FHorzScrollBar: TScrollBar;
    function GetScale: Integer;
    procedure SetBitmap ( const AValue: TBitmap ) ;
    procedure SetMode ( const AValue: TViewermode ) ;
    procedure SetPicture ( const AValue: TLeptPix ) ;
    procedure SetScale ( const AValue: Integer ) ;
    procedure SetupScrollbars;
    procedure OnScrollBarChange( Sender: TObject );
    procedure ResetScrollBars;
  protected
    procedure Paint; override;
    function PictureLoaded: Boolean;
    procedure CalculateLayout;
    procedure Resize; override;
    procedure ReloadBitmap;
    procedure MouseDown ( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
      override;
    procedure MouseMove ( Shift: TShiftState; X, Y: Integer ) ; override;
    procedure MouseUp ( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
      override;
  public
    { public declarations }
    constructor Create ( TheOwner: TComponent ) ; override;
    destructor Destroy; override;
    property Picture: TLeptPix read FCurrentPagePix write SetPicture;
    property OnChangeBitmap: TNotifyEvent read FOnChangeBitmap write FOnChangeBitmap;
    property Mode: TViewermode read FMode write SetMode;
    property Scale: Integer read GetScale write SetScale;
    property Selection: TRect read FSelectRect;
  end;


  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure Button3Click ( Sender: TObject ) ;
    procedure Button4Click ( Sender: TObject ) ;
    procedure Button5Click ( Sender: TObject ) ;
    procedure Button6Click ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
    procedure PaintCanvas(Sender: TObject);
  end;

var
  Form1: TForm1;
  ICanvas : TImageCanvas;
  Image1:TImage;

implementation

{$R *.lfm}

procedure TImageCanvas.SetBitmap ( const AValue: TBitmap ) ;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  FBitmap := AValue;
  if Assigned(FOnChangeBitmap)
      then FOnChangeBitmap(Self);
end;

function TImageCanvas.GetScale: Integer;
begin
  Result := Integer(FScale*100);
end;

procedure TImageCanvas.SetMode ( const AValue: TViewermode ) ;
begin
  if AValue<>FMode then
        begin
          FMode := AValue;
          ReloadBitmap;
          ResetScrollBars;
        end;
end;

procedure TImageCanvas.SetPicture ( const AValue: TLeptPix ) ;
begin
  if AValue<>FCurrentPagePix then
        begin
          FCurrentPagePix := AValue;
          if pixGetDimensions(FCurrentPagePix, @FPageWidth, @FPageHeight, nil)<>0
              then WriteLn('Error when getting image dimensions');
          ReloadBitmap;
        end;
end;

procedure TImageCanvas.SetScale ( const AValue: Integer ) ;
var
  ValueAsFloat: Single;
begin
  if AValue<1 then exit;
  ValueAsFloat := AValue/100;
  if ValueAsFloat<>FScale then
        begin
          FScale := ValueAsFloat;
          if FScale=1
              then FMode := vmFullSize
              else FMode := vmScaled;
          ReloadBitmap;
        end;
end;

procedure TImageCanvas.SetupScrollbars;
begin
  if PictureLoaded then
  begin
    FHorzScrollBar.Visible := (FPageWidth*FScale)>Width+1;
    FVertScrollBar.Visible := (FPageHeight*FScale)>Height+1;
    if FHorzScrollBar.Visible then
          begin
            if FVertScrollBar.Visible
                then FHorzScrollBar.Width := Width-FVertScrollBar.Width
                else FHorzScrollBar.Width := Width;
            FHorzScrollBar.PageSize := Width;
            FHorzScrollBar.Max := FBitmap.Width-Width;
          end;
    if FVertScrollBar.Visible then
          begin
            if FHorzScrollBar.Visible
                then FVertScrollBar.Height := Height-FHorzScrollBar.Height
                else FVertScrollBar.Height := Height;
            FVertScrollBar.PageSize := Height;
            FVertScrollBar.Max := FBitmap.Height-Height;
          end;
  end;
end;

procedure TImageCanvas.OnScrollBarChange ( Sender: TObject ) ;
begin
  Invalidate;
end;

procedure TImageCanvas.ResetScrollBars;
begin
  FVertScrollBar.Position := 0;
  FHorzScrollBar.Position := 0;
end;

procedure TImageCanvas.Paint;
var
  SourceRect: TRect;
  ViewOffsetX: Integer;
  ViewOffsetY: Integer;
begin
if PictureLoaded then
//   if assigned(FBitmap) then
      begin
        ViewOffsetX := FHorzScrollBar.Position;
        ViewOffsetY := FVertScrollBar.Position;
        CalculateLayout;
        SourceRect := Rect(ViewOffsetX, ViewOffsetY, ViewOffsetX+Width, ViewOffsetY+Height);
        Canvas.CopyRect(FClientRect, FBitmap.Canvas, SourceRect);
        Canvas.DrawFocusRect(FSelectRect);
      end;
end;

function TImageCanvas.PictureLoaded: Boolean;
  Begin
    try
      result := assigned(FCurrentPagePix)
      and assigned(FBitmap)
      and (FBitmap.Empty=False);
    except
      on exception do
      result := False;
    end;
  end;

procedure TImageCanvas.CalculateLayout;
begin
  FClientRect.Top := 0;
  FClientRect.Left := 0;
  if FVertScrollBar.Visible
      then FClientRect.Right := FVertScrollBar.Left
      else FClientRect.Right := Width-1;
  if FHorzScrollBar.Visible
      then FClientRect.Bottom := FHorzScrollBar.Top
      else FClientRect.Bottom := Height-1;
end;

procedure TImageCanvas.Resize;
begin
  inherited Resize;
  FVertScrollBar.Left := Width - FVertScrollBar.Width;
  FHorzScrollBar.Top := Height - FHorzScrollBar.Height;
  ReloadBitmap;
end;

procedure TImageCanvas.ReloadBitmap;
begin
//  if PictureLoaded then
  begin
    if FBitmap<>nil then FBitmap.Free;
    FBitmap := TBitmap.Create;
    case FMode of
         vmFitToWidth: begin
                         FScale := Width / FPageWidth;
                       end;
         vmFitToHeight:begin
                         FScale := Height / FPageHeight;
                       end;
         vmFullSize:   begin
                         FScale := 1;
                       end;
    end;
    ScaleToBitmap(FCurrentPagePix, FBitmap, FScale);
    writeln('w:', FPageWidth, '  h:', FPageHeight, ' scale:', FScale);
    SetupScrollbars;
    Invalidate;
  end;
end;

procedure TImageCanvas.MouseDown ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  if mbLeft in [Button] then
        begin
          FSelecting := true;
          FSelectRect.TopLeft := Point(X, Y);
          FSelectRect.BottomRight := Point(X, Y);
        end;
  //inherited MouseDown ( Button, Shift, X, Y ) ;
  Invalidate;
end;

procedure TImageCanvas.MouseMove ( Shift: TShiftState; X, Y: Integer ) ;
begin
  //inherited MouseMove ( Shift, X, Y ) ;
  if FSelecting then
        begin
          FSelectRect.BottomRight := Point(X,Y);
          Invalidate;
        end;
end;

procedure TImageCanvas.MouseUp ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  inherited MouseUp ( Button, Shift, X, Y ) ;
  FSelecting := false;
end;

constructor TImageCanvas.Create ( TheOwner: TComponent ) ;
begin
  inherited Create ( TheOwner ) ;
  FVertScrollBar := TScrollBar.Create(Self);
  FVertScrollBar.Parent := TWinControl(Self);
  FVertScrollBar.Visible := false;
  FVertScrollBar.Kind := sbVertical;
  FVertScrollBar.Top := 0;
  FVertScrollBar.OnChange := @OnScrollBarChange;
  FHorzScrollBar := TScrollBar.Create(Self);
  FHorzScrollBar.Parent := TWinControl(Self);
  FHorzScrollBar.Visible := false;
  FHorzScrollBar.Left := 0;
  FHorzScrollBar.OnChange := @OnScrollBarChange;
  FMode := vmFitToWidth;
  Width := 200;
  Height := 100;
end;

destructor TImageCanvas.Destroy;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  inherited Destroy;
end;

{ TForm1 }

procedure TForm1.Button1Click ( Sender: TObject ) ;
begin
  ICanvas := TImageCanvas.Create(self);
  ICanvas.Parent := Self;
  ICanvas.Top := 100;
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
begin
  if OpenDialog1.Execute
                        then Image1.Picture.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.Button3Click ( Sender: TObject ) ;
begin
  ICanvas.Height := ICanvas.Height+10;
end;

procedure TForm1.Button4Click ( Sender: TObject ) ;
begin
  if OpenDialog1.Execute
                        then  begin
                          writeln('loading');
                          ICanvas.Picture := pixRead(PChar(OpenDialog1.FileName));
                          writeln('loaded');
                        end;

end;

procedure TForm1.Button5Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToHeight;
end;

procedure TForm1.Button6Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToWidth;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TImageCanvas.Create(self);
  ICanvas.Parent := Panel5;
  ICanvas.Align := alClient;
end;

procedure TForm1.PaintCanvas ( Sender: TObject ) ;
begin
end;

end.

