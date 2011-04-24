unit pageviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, leptonica, LibLeptUtils;

type

TViewermode = (vmFitToWindow, vmFitToWidth, vmFitToHeight, vmScaled, vmFullSize);

{ TPageViewer }

TPageViewer = class (TCustomControl)
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


implementation

procedure TPageViewer.SetBitmap ( const AValue: TBitmap ) ;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  FBitmap := AValue;
  if Assigned(FOnChangeBitmap)
      then FOnChangeBitmap(Self);
end;

function TPageViewer.GetScale: Integer;
begin
  Result := Integer(FScale*100);
end;

procedure TPageViewer.SetMode ( const AValue: TViewermode ) ;
begin
  if AValue<>FMode then
        begin
          FMode := AValue;
          ReloadBitmap;
          ResetScrollBars;
        end;
end;

procedure TPageViewer.SetPicture ( const AValue: TLeptPix ) ;
begin
  if AValue<>FCurrentPagePix then
        begin
          FCurrentPagePix := AValue;
          if pixGetDimensions(FCurrentPagePix, @FPageWidth, @FPageHeight, nil)<>0
              then WriteLn('Error when getting image dimensions');
          ReloadBitmap;
        end;
end;

procedure TPageViewer.SetScale ( const AValue: Integer ) ;
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

procedure TPageViewer.SetupScrollbars;
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

procedure TPageViewer.OnScrollBarChange ( Sender: TObject ) ;
begin
  Invalidate;
end;

procedure TPageViewer.ResetScrollBars;
begin
  FVertScrollBar.Position := 0;
  FHorzScrollBar.Position := 0;
end;

procedure TPageViewer.Paint;
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

function TPageViewer.PictureLoaded: Boolean;
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

procedure TPageViewer.CalculateLayout;
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

procedure TPageViewer.Resize;
begin
  inherited Resize;
  FVertScrollBar.Left := Width - FVertScrollBar.Width;
  FHorzScrollBar.Top := Height - FHorzScrollBar.Height;
  ReloadBitmap;
end;

procedure TPageViewer.ReloadBitmap;
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

procedure TPageViewer.MouseDown ( Button: TMouseButton; Shift: TShiftState; X,
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

procedure TPageViewer.MouseMove ( Shift: TShiftState; X, Y: Integer ) ;
begin
  //inherited MouseMove ( Shift, X, Y ) ;
  if FSelecting then
        begin
          FSelectRect.BottomRight := Point(X,Y);
          Invalidate;
        end;
end;

procedure TPageViewer.MouseUp ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  inherited MouseUp ( Button, Shift, X, Y ) ;
  FSelecting := false;
end;

constructor TPageViewer.Create ( TheOwner: TComponent ) ;
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

destructor TPageViewer.Destroy;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  inherited Destroy;
end;


end.

