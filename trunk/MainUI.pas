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
    FOnPaint: TNotifyEvent;
    FViewOffset: TPoint;
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
  protected
    procedure Paint; override;
    function PictureLoaded: Boolean;
    procedure CalculateLayout;
    procedure Resize; override;
    procedure ReloadBitmap;
  public
    { public declarations }
    constructor Create ( TheOwner: TComponent ) ; override;
    destructor Destroy; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Picture: TLeptPix read FCurrentPagePix write SetPicture;
    property OnChangeBitmap: TNotifyEvent read FOnChangeBitmap write FOnChangeBitmap;
    property Mode: TViewermode read FMode write SetMode;
    property Scale: Integer read GetScale write SetScale;
  end;


  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    ScrollBar1: TScrollBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure Button3Click ( Sender: TObject ) ;
    procedure Button4Click ( Sender: TObject ) ;
    procedure Button5Click ( Sender: TObject ) ;
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

procedure TImageCanvas.OnScrollBarChange ( Sender: TObject ) ;
begin
  if Sender=FVertScrollBar
      then FViewOffset.Y := FVertScrollBar.Position
  else if Sender=FHorzScrollBar
      then FViewOffset.X := FHorzScrollBar.Position;
  Invalidate;
end;

procedure TImageCanvas.Paint;
var
  SourceRect: TRect;
begin
//  inherited Paint;
if PictureLoaded then
//   if assigned(FBitmap) then
      begin
//        writeln('calculate layout');
        CalculateLayout;
        SourceRect := Rect(FViewOffset.X, FViewOffset.Y, FViewOffset.X+Width, FViewOffset.Y+Height);
//        writeln('start painting');
        Canvas.CopyRect(FClientRect, FBitmap.Canvas, SourceRect);
//        writeln('end painting');
      end;
//writeln('Width: ', Width);
//writeln('Height: ', Height);
//Canvas.Draw(0, 0, FBitmap);
//writeln('exit Paint');
  if Assigned(FOnPaint)
     then FOnPaint(Self);
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
  case FMode of
       vmFitToWindow : begin
                         FVertScrollBar.Visible := false;
                         FHorzScrollBar.Visible := false;

                       end;

       else
       begin

       end;

  end;
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
//  FVertScrollBar.Height := Height - FHorzScrollBar.Height;
  FHorzScrollBar.Top := Height - FHorzScrollBar.Height;
//  FHorzScrollBar.Width := FVertScrollBar.Left;
  SetupScrollbars;
end;

procedure TImageCanvas.ReloadBitmap;
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

constructor TImageCanvas.Create ( TheOwner: TComponent ) ;
begin
  inherited Create ( TheOwner ) ;
  FVertScrollBar := TScrollBar.Create(Self);
  FVertScrollBar.Parent := TWinControl(Self);
  FVertScrollBar.Kind := sbVertical;
  FVertScrollBar.Top := 0;
  FVertScrollBar.OnChange := @OnScrollBarChange;
  FHorzScrollBar := TScrollBar.Create(Self);
  FHorzScrollBar.Parent := TWinControl(Self);
  FHorzScrollBar.Left := 0;
  FHorzScrollBar.OnChange := @OnScrollBarChange;
  FMode := vmFitToWidth;
  FViewOffset := Point(0, 0);
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
  //ICanvas.Picture := Image1.Picture;
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
  //ICanvas.Bitmap := Image1.Picture.Bitmap;
                        end;

end;

procedure TForm1.Button5Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToHeight;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TImageCanvas.Create(self);
  ICanvas.Parent := Panel5;
  ICanvas.Align := alClient;
  {ICanvas.AnchorSide[akLeft].Control := Panel5;
  ICanvas.AnchorSide[akLeft].Side := asrLeft;
  ICanvas.AnchorSide[akTop].Control := Panel5;
  ICanvas.AnchorSide[akTop].Side := asrTop;
  ICanvas.AnchorSide[akRight].Control := ImageVertScrollbar;
  ICanvas.AnchorSide[akRight].Side := asrLeft;
  ICanvas.AnchorSide[akBottom].Control := ImageHorzScrollbar;
  ICanvas.AnchorSide[akBottom].Side := asrTop;
  ICanvas.Anchors := ICanvas.Anchors + [akTop, akLeft, akRight, akBottom];  }

  Image1 := TImage.Create(Self);
end;

procedure TForm1.PaintCanvas ( Sender: TObject ) ;
begin
end;

end.

