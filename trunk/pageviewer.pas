unit pageviewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Graphics, leptonica, LibLeptUtils, selector;

type

TViewermode = (vmFitToWindow, vmFitToWidth, vmFitToHeight, vmScaled, vmFullSize);
TSelectionMode = (smSelect, smCrop, smDelete);

{ TPageViewer }

TPageViewer = class (TCustomControl)
private
  FOnDeleteSelection: TNotifyEvent;
  { private declarations }
  FSelectRect: TRect;
  FSelecting: Boolean;
  FSelectionMode: TSelectionMode;
  FSelections: array of TSelector;
  FRealSelections: array of TRect;
  FOnChangeBitmap: TNotifyEvent;
  FOnSelect: TNotifyEvent;
  FCurrentPagePix: PLPix;
  FPageWidth: Integer;
  FPageHeight: Integer;
  FBitmap: TBitmap;
  FClientRect: TRect;
  FMode: TViewermode;
  FScale: Single;
  FVertScrollBar: TScrollBar;
  FHorzScrollBar: TScrollBar;
  //function GetScale: Integer;
  function GetSelection: TRect;
  function GetSelectionCount: integer;
  procedure SetBitmap ( const AValue: TBitmap ) ;
  procedure SetMode ( const AValue: TViewermode ) ;
  procedure SetPicture ( const AValue: PLPix ) ;
  procedure SetScale ( const AValue: Single ) ;
  procedure SetupScrollbars;
  procedure OnScrollBarChange( Sender: TObject );
  procedure ResetScrollBars;
  function GetSelections( AValue: Integer ): TRect;
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
  procedure SelectionChange ( Sender: TObject ) ;
  procedure SelectionGrabFocus ( Sender: TObject ) ;
  procedure ResizeSelections;
public
  { public declarations }
  constructor Create ( TheOwner: TComponent ) ; override;
  destructor Destroy; override;
  procedure ClearSelection;
  procedure AddSelector( Selector: TSelector );
  procedure DeleteSelector ( SelIndex: Integer );
  function GetThumbnail ( w, h: Integer ): TBitmap;
  property Picture: PLPix read FCurrentPagePix write SetPicture;
  property OnChangeBitmap: TNotifyEvent read FOnChangeBitmap write FOnChangeBitmap;
  property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  property Mode: TViewermode read FMode write SetMode;
  property Scale: Single read FScale write SetScale;
  procedure AddSelection( Selection: TRect );
  property CurrentSelection: TRect read GetSelection;
  property Selections[SelIndex: Integer]: TRect read GetSelections;
  property SelectionMode: TSelectionMode read FSelectionMode write FSelectionMode;
  property SelectionCount: integer read GetSelectionCount;
  property OnDeleteSelection: TNotifyEvent read FOnDeleteSelection write FOnDeleteSelection;
  function SelectionIndex( Sel: TSelector ): integer;
  function GetSelector( SelIndex: integer ): TSelector;
  procedure SetSelection( SelIndex: integer; aRect: TRect );
end;


  function ScaleRect( aRect: TRect; scale: Single ): TRect;
  function ScaleRectToView( aRect: TRect; viewfactor: Single ): TRect;
  function UnScaleRect( aRect: TRect; viewfactor: Single ): TRect;
  function ScaleAndOffsetRect(ARect: TRect; scalexy: single; offsetX, offsetY: Integer): TRect;
  Function  JLReScale(aWidth,aHeight:Integer;
          NewWidth,NewHeight:Integer;
          var outRect:TRect):Boolean;
  procedure AdjustRect(var aRect: TRect); //ensures that BottomRight>TopLeft

implementation

function ScaleRect ( aRect: TRect; scale: Single ) : TRect;
var
  TempRect: TRect;
begin
  {$IFDEF DEBUG}
  writeln('scale is ', FormatFloat( '0.00', scale ));
  writeln( Format('Input rect: %d %d %d %d', [aRect.Top, aRect.Left, aRect.Bottom, aRect.Right]) );
  {$ENDIF}
  TempRect.Top := Round(aRect.Top*scale);
  TempRect.Left := Round(aRect.Left*scale);
  TempRect.Bottom := Round(aRect.Bottom*scale);
  TempRect.Right := Round(aRect.Right*scale);
  Result := TempRect;
  {$IFDEF DEBUG}
  writeln( Format('Output rect: %d %d %d %d', [TempRect.Top, TempRect.Left, TempRect.Bottom, TempRect.Right]) );
  {$ENDIF}
end;

function ScaleRectToView ( aRect: TRect; viewfactor: single ) : TRect;
begin
  Result := ScaleRect( aRect, viewfactor/100 );
  {$IFDEF DEBUG} writeln('ScaleRectToView'); {$ENDIF}
end;

function UnScaleRect ( aRect: TRect; viewfactor: Single ) : TRect;
begin
  Result := ScaleRect( aRect, 1/viewfactor );
  {$IFDEF DEBUG} writeln('UnScaleRect'); {$ENDIF}
end;

function ScaleAndOffsetRect(ARect: TRect; scalexy: single; offsetX, offsetY: Integer): TRect;
var
  TempRect: TRect;
begin
  TempRect.Left := Round(ARect.Left*scalexy) - offsetX;
  TempRect.Top := Round(ARect.Top*scalexy) - offsetY;
  TempRect.Right := Round(ARect.Right*scalexy) - offsetX;
  TempRect.Bottom := Round(ARect.Bottom*scalexy)- offsetY;
  Result := TempRect;
end;

{ JLReScale copyright Jon L. Aasenden http://delphimax.wordpress.com/ }
Function  JLReScale(aWidth,aHeight:Integer;
          NewWidth,NewHeight:Integer;
          var outRect:TRect):Boolean;
var
  x,y:    Integer;
  x1,y1:  Real;
  wd,hd:  Integer;
Begin
  result:=False;
  if (aWidth>1) and (aHeight>1) then
  Begin
    If (NewWidth>1) and (NewHeight>1) then
    Begin
      x1:=(NewWidth/aWidth);
      y1:=(NewHeight/aHeight);
      if x1 > y1 then
      begin
        outRect.top:=0;
        outRect.bottom:=NewHeight;
        x:=trunc(aWidth*y1);
        outRect.left:=(NewWidth-x) shr 1;
        outRect.right:=outRect.left+x;
      end else
      begin
        outRect.left:=0;
        outRect.right:=NewWidth;
        y:=trunc(aHeight*x1);
        outRect.top:=(NewHeight-y) shr 1;
        outRect.bottom:=outRect.top+y;
      end;
      result:=True;
    end;
  end;
end;

procedure AdjustRect ( var aRect: TRect ) ;
var
  i: LongInt;
begin
  with aRect do
       begin
         if Top>Bottom then
               begin
                 i := Top;
                 Top := Bottom;
                 Bottom := i
               end;
         if Left>Right then
               begin
                 i := Left;
                 Left := Right;
                 Right := i;
               end;
       end;
end;


{ TPageViewer }
procedure TPageViewer.SetBitmap ( const AValue: TBitmap ) ;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  FBitmap := AValue;
  if Assigned(FOnChangeBitmap)
      then FOnChangeBitmap(Self);
end;

function TPageViewer.GetSelection: TRect;
begin
  Result.Top := FVertScrollBar.Position + Trunc(FSelectRect.Top/FScale);
  Result.Left := FHorzScrollBar.Position + Trunc(FSelectRect.Left/FScale);
  Result.Bottom := FVertScrollBar.Position + Trunc(FSelectRect.Bottom/FScale);
  Result.Right := FHorzScrollBar.Position + Trunc(FSelectRect.Right/FScale);
end;

function TPageViewer.GetSelectionCount: integer;
begin
  Result := Length(FSelections);
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

procedure TPageViewer.SetPicture ( const AValue: PLPix ) ;
var
  x: Integer;
begin
  if AValue<>FCurrentPagePix then
        begin
          FCurrentPagePix := AValue;
          if FCurrentPagePix<>nil then
             if pixGetDimensions(FCurrentPagePix, @FPageWidth, @FPageHeight, nil)<>0
                then {$IFDEF DEBUG} WriteLn('Error when getting image dimensions') {$ENDIF};
          For x := Length(FSelections)-1 downto 0 do
            FSelections[x].Free;
          SetLength(FSelections, 0);
          SetLength(FRealSelections, 0);
          ClearSelection;
          ReloadBitmap;
          if Assigned(FOnChangeBitmap)
              then FOnChangeBitmap(Self);
        end;
end;

procedure TPageViewer.SetScale ( const AValue: Single ) ;
begin
  if AValue<0.05 then exit;
  if AValue<>FScale then
        begin
          FScale := AValue;
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
var
  x: Integer;
begin
  for x := 0 to Length(FRealSelections)-1 do
    FSelections[x].Selection := ScaleAndOffsetRect(FRealSelections[x], FScale, FHorzScrollBar.Position, FVertScrollBar.Position);
  Invalidate;
end;

procedure TPageViewer.ResetScrollBars;
begin
  FVertScrollBar.Position := 0;
  FHorzScrollBar.Position := 0;
end;

function TPageViewer.GetSelections ( AValue: Integer ) : TRect;
begin

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
        if FSelectionMode=smSelect
           then  Canvas.DrawFocusRect(FSelectRect)
        else begin
             Canvas.Brush.Style := bsClear;
             Canvas.Rectangle(FSelectRect);
           end;
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
//  ReloadBitmap;
end;

procedure TPageViewer.ReloadBitmap;
begin
//  if PictureLoaded then
  begin
    if FBitmap<>nil then
       begin
         FBitmap.Free;
         FBitmap := nil;
       end;
    if FCurrentPagePix<> nil then
        begin
          FBitmap := TBitmap.Create;
          case FMode of
               vmFitToWidth: begin
                               FScale := Width / FPageWidth;
                               if (FPageHeight*FScale) > Height
                                  then FScale := (Width-FVertScrollBar.Width) / FPageWidth;
                             end;
               vmFitToHeight:begin
                               FScale := Height / FPageHeight;
                               if (FPageWidth*FScale) > Width
                                  then FScale := (Height-FHorzScrollBar.Height) / FPageHeight;
                             end;
               vmFullSize:   begin
                               FScale := 1;
                             end;
          end;
          ScaleToBitmap(FCurrentPagePix, FBitmap, FScale);
          {$IFDEF DEBUG}
          writeln('w:', FPageWidth, '  h:', FPageHeight, ' scale:', FormatFloat('0.00', FScale));
          {$ENDIF}
          SetupScrollbars;
          ResizeSelections;
          Invalidate;
        end;
  end;
end;

procedure TPageViewer.MouseDown ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  if FBitmap=nil then exit;
  if mbLeft in [Button] then
        begin
          FSelecting := true;
          FSelectRect.TopLeft := Point(X, Y);
          FSelectRect.BottomRight := Point(X, Y);
        end;
  //inherited MouseDown ( Button, Shift, X, Y ) ;
//  writeln('MouseDown: ', x, #32, y);
  Invalidate;
end;

procedure TPageViewer.MouseMove ( Shift: TShiftState; X, Y: Integer ) ;
begin
  if FBitmap=nil then exit;
  if FSelecting then
        begin
          FSelectRect.BottomRight := Point(X,Y);
          Invalidate;
        end;
end;

procedure TPageViewer.MouseUp ( Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer ) ;
begin
  if FBitmap=nil then exit;
  inherited MouseUp ( Button, Shift, X, Y ) ;
  if FSelecting then
        begin
          AdjustRect(FSelectRect);
          if Assigned(FOnSelect)
            then FOnSelect( Self );
        end;
  FSelecting := false;
  FSelectRect.TopLeft := Point(-1,-1);
  FSelectRect.BottomRight := Point(-1,-1);
end;

procedure TPageViewer.SelectionChange(Sender: TObject);
begin
  if FSelectionMode=smSelect
     then FRealSelections[ SelectionIndex( TSelector(Sender) ) ] := UnScaleRect(TSelector(Sender).Selection, Scale);
  {$IFDEF DEBUG}
  writeln('TPageViewer received SelectionChange from Selector index ', SelectionIndex( TSelector(Sender) ));
  {$ENDIF}
end;

procedure TPageViewer.SelectionGrabFocus ( Sender: TObject ) ;
var
  x: Integer;
begin
  if FSelectionMode=smSelect then
    begin
      for x := 0 to Length(FSelections)-1 do
        if FSelections[x]<>Sender then begin FSelections[x].Focussed := false; FSelections[x].Invalidate; end;
    end
  else if FSelectionMode=smDelete then
     begin
       if Assigned(FOnDeleteSelection)
         then FOnDeleteSelection(Sender);
       DeleteSelector(SelectionIndex(TSelector(Sender)));
     end;
end;

procedure TPageViewer.ResizeSelections;
var
  x: Integer;
begin
  for x := 0 to Length(FSelections)-1 do
    FSelections[x].Selection := ScaleRect(FRealSelections[x], FScale);
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
  FSelectionMode := smSelect;
  Width := 200;
  Height := 100;
end;

destructor TPageViewer.Destroy;
var
  X: Integer;
begin
  if Assigned(FBitmap)
     then FBitmap.Free;
  inherited Destroy;
end;

procedure TPageViewer.ClearSelection;
begin
  with FSelectRect do
       begin
         TopLeft := Point(0, 0);
         BottomRight := Point(0, 0);
       end;
  Invalidate;
end;

procedure TPageViewer.AddSelector ( Selector: TSelector ) ;
begin
  {$IFDEF DEBUG} writeln(Selector.Name, '  ', Selector.Top); {$ENDIF}
  SetLength(Fselections, Length(Fselections)+1);
  Fselections[ Length( Fselections )-1 ] := Selector;
  {$IFDEF DEBUG}
  writeln(Fselections[0].Selection.Top);
  {$ENDIF}
end;

procedure TPageViewer.DeleteSelector ( SelIndex: Integer ) ;
var
  x: LongInt;
begin
  if (SelIndex>=0) and (SelIndex<Length(Fselections)) then
        begin
          Fselections[SelIndex].Free;
          for x := SelIndex to Length(Fselections)-2 do
            begin
              Fselections[x] := Fselections[x+1];
              FRealSelections[x] := FRealSelections[x+1];
            end;
          SetLength(Fselections, Length(Fselections)-1);
          SetLength(FRealSelections, Length(Fselections));
          {$IFDEF DEBUG}
          writeln('reconfigured selections');
          {$ENDIF}
        end
  else {$IFDEF DEBUG} WriteLn(Format('Error in DeleteSelector: Index out of range (%d)', [SelIndex])) {$ENDIF};
  //Raise Exception.CreateFmt('Error in DeleteSelector: Index out of range (%d)', [SelIndex]);
end;

function TPageViewer.GetThumbnail ( w, h: Integer ) : TBitmap;
var
  TargetRect: TRect;
  Thumbnail: TBitmap;
begin
  Result := nil;
  if JLReScale(FBitmap.Width, FBitmap.Height, w, h, TargetRect) then
        begin
          {$IFDEF DEBUG}
          writeln( Format('Rect: %d %d %d %d ', [TargetRect.Left, TargetRect.Top, TargetRect.Right, TargetRect.Bottom]));
          {$ENDIF}
          //now remove left and top borders if any
          TargetRect.Right := TargetRect.Right-TargetRect.Left;
          TargetRect.Left := 0;
          TargetRect.Bottom := TargetRect.Bottom-TargetRect.Top;
          TargetRect.Top := 0;
          Thumbnail := TBitmap.Create;
          Thumbnail.Width := TargetRect.Right;
          Thumbnail.Height := TargetRect.Bottom;
          Thumbnail.Canvas.StretchDraw(TargetRect, FBitmap);
          Result := Thumbnail;
        end;
end;

procedure TPageViewer.AddSelection(Selection: TRect);
var
  S: TSelector;
begin
  S := TSelector.Create(Self);
  S.Parent := Self;
  S.Color := clGreen;
  S.Width := 2;
  S.Selection := ScaleRect(Selection, FScale);
  S.OnSelect := @SelectionChange;
  S.OnFocus := @SelectionGrabFocus;
  SetLength(FRealSelections, Length(FRealSelections)+1);
  FRealSelections[Length(FRealSelections)-1] := Selection;
  SetLength(FSelections, Length(FSelections)+1);
  FSelections[ Length( FSelections )-1 ] := S;
  S.Caption := IntToStr(Length(FSelections));
  S.SetFocus;
end;

function TPageViewer.SelectionIndex(Sel: TSelector): integer;
var
  x: Integer;
begin
  Result := -1;
  x := 0;
  while (x<Length(FSelections)) and (Result<0) do
        begin
        if FSelections[x]=Sel then Result := x;
        inc(x);
        end;
end;

function TPageViewer.GetSelector(SelIndex: integer): TSelector;
begin
  Result := nil;
  if (SelIndex<0) or (SelIndex>Length(FSelections)-1) then exit;
  Result := FSelections[SelIndex];
end;

procedure TPageViewer.SetSelection(SelIndex: integer; aRect: TRect);
begin
  if (SelIndex<0) or (SelIndex>Length(FSelections)-1) then exit;
   FRealSelections[ SelIndex ] := UnScaleRect(aRect, Scale);
end;


end.

