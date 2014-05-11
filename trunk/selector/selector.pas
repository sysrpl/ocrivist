unit selector;

// A frame selector component
// for selecting areas of a window

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, types, LMessages;

{ TSelector }

type
TResizeHandle = (rhNone, rhTopLeft, rhTopRight, rhBottomLeft, rhBottomRight);

TSelector = class (TGraphicControl)
  private
    FAspect: Single;
    FOnDelete: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    fFocussed: Boolean;
    FOnFocus: TNotifyEvent;
    fResizing: Boolean;
    fStartX: Integer;
    fStartY: integer;
    fStartTop: integer;
    fStartLeft: Integer;
    fStartHeight: Integer;
    fStartWidth: Integer;
    fClientRect: TRect;
    fResizehandle: TResizeHandle;
    fCaption: String;
    fCaptionHeight: integer;
    fTopLeftHandle: TRect;
    fTopRightHandle: TRect;
    fBottomLeftHandle: TRect;
    fBottomRightHandle: TRect;
    fColor: TColor;
    fResizeColor: TColor;
    fPenSize: Integer;
    fDrawHandles: Boolean;
    FUserData: Pointer;
    function GetSelection: TRect;
    procedure SetAspect ( AValue: Single ) ;
    procedure SetCaption ( const AValue: string ) ;
    procedure SetColor ( const AValue: TColor ) ;
    procedure SetDrawHandles ( const AValue: Boolean ) ;
    procedure SetFocussed ( AValue: Boolean ) ;
    procedure SetPenSize ( const AValue: Integer ) ;
    procedure SetSelection ( const AValue: TRect ) ;
    procedure WMMouseLeave(var Msg: TLMessage); message CM_MOUSELEAVE;
  protected
    procedure SelectorMouseDown ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer ) ; virtual;
    procedure SelectorMouseMove ( Sender: TObject; Shift: TShiftState; X,
      Y: Integer ) ; virtual;
    procedure SelectorMouseUp ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer ) ; virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus;
    property Selection: TRect read GetSelection write SetSelection;
    property Caption: string read fCaption write SetCaption;
    property Color: TColor read fColor write SetColor;
    property ResizeColor: TColor read fResizeColor write fResizeColor;
    property LineWidth: Integer read fPenSize write SetPenSize;
    property DrawHandles: Boolean read fDrawHandles write SetDrawHandles;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnFocus: TNotifyEvent read FOnFocus write FOnFocus;
    property Focussed: Boolean read fFocussed write SetFocussed;
    property UserData: Pointer read FUserData write FUserData;
    property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
    property Aspect: Single read FAspect write SetAspect default 0.0;
  end;

PSelector = ^TSelector;

const
  CAPTIONPADDING =  2;
  HANDLESIZE     = 10;

implementation

{ TSelector }

procedure TSelector.Paint;
    procedure PositionHandles;
    begin
      with fTopLeftHandle do
           begin
             Left := fClientRect.Left;
             Top := fClientRect.Top;
             Bottom := Top + HANDLESIZE;
             Right := Left + HANDLESIZE;
           end;
      with fTopRightHandle do
           begin
             Left := fClientRect.Right-HANDLESIZE;
             Top := fClientRect.Top;
             Bottom := Top + HANDLESIZE;
             Right := Left + HANDLESIZE;
           end;
      with fBottomLeftHandle do
           begin
             Left := fClientRect.Left;
             Top := fClientRect.Bottom-HANDLESIZE;
             Bottom := Top + HANDLESIZE;
             Right := Left + HANDLESIZE;
           end;
      with fBottomRightHandle do
           begin
             Left := fClientRect.Right-HANDLESIZE;
             Top := fClientRect.Bottom-HANDLESIZE;
             Bottom := Top + HANDLESIZE;
             Right := Left + HANDLESIZE;
           end;
    end;
begin
//  inherited Paint;
    //Canvas.pen.Mode := pm;
//  writeln('painting ', Self.Name);
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := clNone;
  {if fResizing then Canvas.Pen.Color := fResizeColor
  else }Canvas.Pen.Color := fColor;
  Canvas.Pen.Width := fPenSize;
  if fFocussed then Canvas.Pen.Width := fPenSize + 2
  else Canvas.Pen.Width := fPenSize;

  with fClientRect do
         begin
           Top := fCaptionHeight + CAPTIONPADDING;
           Left := fPenSize;
           Right := Self.Width-1;
           Bottom := Self.Height-1;
         end;
    PositionHandles;

    Canvas.Rectangle(fClientRect);
    Canvas.Pen.Width := 1;
    if fDrawHandles then
      begin
        Canvas.Rectangle(fTopLeftHandle);
        Canvas.Rectangle(fTopRightHandle);
        Canvas.Rectangle(fBottomLeftHandle);
        Canvas.Rectangle(fBottomRightHandle);
      end;
    Canvas.Font.Color := fColor;
    Canvas.TextOut(0, 0, fCaption);
end;

constructor TSelector.Create ( AOwner: TComponent ) ;
begin
  inherited Create ( AOwner ) ;
  OnMouseDown := @SelectorMouseDown;
  OnMouseMove := @SelectorMouseMove;
  OnMouseUp := @SelectorMouseUp;
  Top := 50;
  Left := 50;
  fCaption := '';
  fColor := clBlack;
  fResizeColor := clBlack;
  fPenSize := 1;
end;

destructor TSelector.Destroy;
begin
  if Assigned(FOnDelete)
     then FOnDelete(Self);
  {$IFDEF DEBUG}
  WriteLn('TSelector destroyed');
  {$ENDIF}
  inherited Destroy;
end;

procedure TSelector.SetFocus;
begin
  fFocussed := true;
  Paint;
  {$IFDEF DEBUG}
  writeln(fCaption, ' SetFocus');
  {$ENDIF}
  if Assigned(FOnFocus) then FOnFocus(Self);
end;

procedure TSelector.SelectorMouseDown ( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer ) ;
var
  MouseCoords: TPoint;
begin
   MouseCoords.X := X;
   MouseCoords.Y := Y;
   fResizehandle := rhNone;
   if PtInRect(fTopLeftHandle, MouseCoords)
      then fResizehandle := rhTopLeft;
   if PtInRect(fTopRightHandle, MouseCoords)
      then fResizehandle := rhTopRight;
   if PtInRect(fBottomLeftHandle, MouseCoords)
      then fResizehandle := rhBottomLeft;
   if PtInRect(fBottomRightHandle, MouseCoords)
      then fResizehandle := rhBottomRight;

   {$IFDEF DEBUG}
   case fResizehandle of
        rhNone: writeln('rhNone');
        rhTopLeft: writeln('rhTopLeft');
        rhBottomLeft: writeln('rhBottomLeft');
        rhTopRight: writeln('rhTopRight');
        rhBottomRight: writeln('rhBottomRight');
   end;
   {$ENDIF}
   if fResizehandle<>rhNone
      then fResizing := true;
   fStartX := X;
   fStartY := Y;
   fStartTop := Top;
   fStartLeft := Left;
   fStartHeight := Height;
   fStartWidth := Width;
   Self.SetFocus;
end;

function TSelector.GetSelection: TRect;
var
  R: TRect;
begin
  R.Top := Top + fCaptionHeight + CAPTIONPADDING;
  R.Bottom := Top + Height;
  R.Left := Left +1;
  R.Right := R.Left + Width;
  Result := R;
end;

procedure TSelector.SetAspect ( AValue: Single ) ;
begin
  if FAspect = AValue then Exit;
  FAspect := AValue;
end;

procedure TSelector.SelectorMouseMove ( Sender: TObject; Shift: TShiftState; X,
  Y: Integer ) ;
var
  MouseCoords: TPoint;
  heightdiff: Integer;
  widthdiff: Integer;

  procedure ResetCoords;
  begin
   fStartX := X;
   fStartY := Y;
   fStartTop := Top;
   fStartLeft := Left;
   fStartHeight := Height;
   fStartWidth := Width;
  end;

  procedure HandleCollision( PP: TPoint; H: TRect ; newHandle: TResizeHandle);
  begin
    if PtInRect(H, PP) then
      begin
        fResizehandle := newHandle;
        ResetCoords;
      end;
  end;

begin
   MouseCoords.X := X;
   MouseCoords.Y := Y;
   Self.BringToFront;
   if fResizing then
     begin
       heightdiff := Y-fStartY;
       widthdiff  := X-fStartX;
       case fResizehandle of
           rhTopLeft: begin
                        Top := Top + (Y-fStartY);
                        Left := Left + (X-fStartX);
                        Height := fStartHeight - (Top-fStartTop);
                        Width := fStartWidth - (Left-fStartLeft);
                        {HandleCollision(fTopLeftHandle.BottomRight, fTopRightHandle, rhTopRight);
                        HandleCollision(fTopLeftHandle.BottomRight, fBottomLeftHandle, rhBottomLeft);
                        HandleCollision(fTopLeftHandle.BottomRight, fBottomRightHandle, rhBottomRight);}
                      end;
           rhTopRight: begin
                         Top := Top + (Y-fStartY);
                         Height := fStartHeight - (Top-fStartTop);
                         Width := fStartWidth + (X-fStartX);
                         {HandleCollision(fTopLeftHandle, rhTopLeft);
                         HandleCollision(fBottomRightHandle, rhBottomRight);
                         HandleCollision(fBottomLeftHandle, rhBottomLeft);}
                      end;
           rhBottomLeft: begin
                        Left := Left + (X-fStartX);
                        Height := fStartHeight + (Y-fStartY);
                        Width := fStartWidth - (Left-fStartLeft);
                        {HandleCollision(fTopLeftHandle, rhTopLeft);
                        HandleCollision(fBottomRightHandle, rhBottomRight);
                        HandleCollision(fTopRightHandle, rhTopRight);}
                     end;
           rhBottomRight: begin
                       Height := fStartHeight + (Y-fStartY);

                       if FAspect<>0.0 then
                         Width := Round(Height * FAspect)
                         else Width := fStartWidth + (X-fStartX);
                       {HandleCollision(fTopRightHandle, rhTopRight);
                       HandleCollision(fBottomLeftHandle, rhBottomLeft);
                       HandleCollision(fTopLeftHandle, rhTopLeft);    }
                     end;
      end;
   if (Width < (HANDLESIZE * 2)) or (Height < ((HANDLESIZE * 2) + CAPTIONPADDING + fCaptionHeight))
      then fResizing := false;
     end
   else
   begin
    if PtInRect(fTopLeftHandle, MouseCoords)
      then Cursor := crSizeNW
   else if PtInRect(fTopRightHandle, MouseCoords)
      then Cursor := crSizeNE
   else if PtInRect(fBottomLeftHandle, MouseCoords)
      then Cursor := crSizeSW
   else if PtInRect(fBottomRightHandle, MouseCoords)
      then Cursor := crSizeSE
   else Cursor := crDefault;
   end;


end;

procedure TSelector.SelectorMouseUp ( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer ) ;
begin
  fResizing := false;
  if fResizehandle<>rhNone then
    begin
      if Assigned(FOnSelect)
         then FOnSelect(Self);
      Paint;
    end;
end;

procedure TSelector.SetCaption ( const AValue: string ) ;
begin
  fCaption := AValue;
  Self.Paint;
end;

procedure TSelector.SetColor ( const AValue: TColor ) ;
begin
  if fColor = AValue then Exit;
  fColor := AValue;
  Paint;
end;

procedure TSelector.SetDrawHandles ( const AValue: Boolean ) ;
begin
  if fDrawHandles = AValue then Exit;
  fDrawHandles := AValue;
  Paint;
end;

procedure TSelector.SetFocussed ( AValue: Boolean ) ;
begin
  if fFocussed = AValue then Exit;
  fFocussed := AValue;
  if fFocussed then SetFocus;
  Invalidate;
end;

procedure TSelector.SetPenSize ( const AValue: Integer ) ;
begin
  if fPenSize = AValue then Exit;
  fPenSize := AValue;
  Invalidate;
end;

procedure TSelector.SetSelection ( const AValue: TRect ) ;
begin
  fCaptionHeight := Canvas.TextHeight('Yy');
  Self.Top := AValue.Top - fCaptionHeight - CAPTIONPADDING;
  Left := AValue.Left;
  Height := AValue.Bottom - AValue.Top + fCaptionHeight + CAPTIONPADDING;
  Width := AValue.Right - AValue.Left;
end;

procedure TSelector.WMMouseLeave ( var Msg: TLMessage ) ;
begin
  Cursor := crDefault;
end;



end.

