unit scantwain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, DelphiTwain, scanner, leptonica, Graphics, IntfGraphics;

var
  Twain: TDelphiTwain;
  TwainSource: TTwainSource;


type

{ TTwainScanner }

TTwainScanner = class (TObject)
  public
    constructor Create;
    destructor Destroy; override;
    function ScanToPix: PLPix;
    procedure TwainTwainAcquire(Sender: TObject; const {%H-}Index: Integer;
      Image: TBitmap; var Cancel: Boolean);
  end;

var
  TwainScanner: TTwainScanner;
  ScannedPix: PLPix;
  FScanning: Boolean;

function SelectDevice: integer;
function GetSelectedScannerSettings: integer;
function SetScannerOptions: Integer;

implementation

function SelectDevice: integer;
var
  SelectedSource: Integer;
begin
  if not Assigned(Twain) then begin
    Twain := TDelphiTwain.Create(nil);
    Twain.OnTwainAcquire := @TwainScanner.TwainTwainAcquire;
  end;
  {It is always recommended to load library dynamically, never forcing}
  {final user to have twain installed}
  if Twain.LoadLibrary then
  begin

    {Load source manager}
    Twain.SourceManagerLoaded := TRUE;
    {Allow user to select source}
    SelectedSource := Twain.SelectSource;
    TwainSource := Twain.Source[SelectedSource];
  end;
  Result := SelectedSource;
end;

function GetSelectedScannerSettings: integer;
var
  xr: Extended;
  maxw, maxh: Extended;
  xr_range: TTwainResolution;
  x: TCapabilityRet;
  pixtype: TTwainPixelType;
  pixcap: TTwainPixelTypeSet;
  c: Integer;
  hasfeeder: Boolean;
  u: TTwainUnit;
  uset: TTwainUnitSet;
begin
  with ScannerForm do
       begin
         NameLabel.Caption := TwainSource.Manufacturer + #32 + TwainSource.ProductName;
         TwainSource.ShowUI := false;
         TwainSource.Loaded := true;
         try
//         x := TwainSource.GetCaps;
         x := TwainSource.GetIXResolution(xr, xr_range);
         if x = crSuccess then
            begin
              InitialiseResolution(Round(xr_range[Low(xr_range)]), Round(xr_range[High(xr_range)]), Round(xr));
            end;
          x := TwainSource.GetIPixelType(pixtype, pixcap);
          if x = crSuccess then
             begin
               ModeComboBox.Clear;
               if tbdBw in pixcap then ModeComboBox.Items.Add('Lineart');
               if tbdGray in pixcap then ModeComboBox.Items.Add('Gray');
               if tbdRgb in pixcap then ModeComboBox.Items.Add('Color');
               if pixtype=tbdBw then ModeComboBox.ItemIndex := ModeComboBox.Items.IndexOf('Lineart')
               else if pixtype=tbdGray then ModeComboBox.ItemIndex := ModeComboBox.Items.IndexOf('Gray')
               else if pixtype=tbdRgb then ModeComboBox.ItemIndex := ModeComboBox.Items.IndexOf('Color'); ;
               ModeComboBox.Enabled := ModeComboBox.Items.Count>0;
             end;
          hasfeeder := false;
          SourceComboBox.Clear;
          SourceComboBox.Items.Add('Flatbed');
          x := TwainSource.GetAutofeed(hasfeeder);
          if x = crSuccess then
             if hasfeeder then SourceComboBox.Items.Add('Autofeeder');
          SourceComboBox.ItemIndex := 0;
          x := TwainSource.GetICapUnits(u, uset);
          x := TwainSource.GetIPhysicalWidth(maxw);
          x := TwainSource.GetIPhysicalHeight(maxh);
          case u of
               tuInches: begin maxw := maxw * 25.4; maxh := maxh * 25.4; end;
               tuCentimeters: begin maxw := maxw * 10; maxh := maxh * 10; end;
               tuPixels: begin maxw := maxw / xr * 25.4; maxh := maxh / xr * 25.4; end;
          end;
          HeightSpinEdit.MaxValue := maxh;
          WidthSpinEdit.MaxValue := maxw;
          PaperFormatBoxChange(nil);
         finally
           TwainSource.Loaded := false;
         end;
       end;
end;

function SetScannerOptions: Integer;
var
  res: TCapabilityRet;
  autofeed: WordBool;
  resoln: Extended;
  H, W: Double;
  u: TTwainUnit;
  uset: TTwainUnitSet;
begin
  if Assigned(TwainSource) then
  with ScannerForm do
       begin
         resoln := ScannerForm.GetResolution;
         res := TwainSource.SetIXResolution(resoln);
         res := TwainSource.SetIYResolution(resoln);
         case GetColorMode of
              'Lineart' : res := TwainSource.SetIPixelType(tbdBw);
              'Gray'    : res := TwainSource.SetIPixelType(tbdGray);
              'Color'   : res := TwainSource.SetIPixelType(tbdPalette);
         end;
         res := TwainSource.SetAutoFeed(GetSource='Autofeeder');
         H := HeightSpinEdit.Value;
         W := WidthSpinEdit.Value;
         res := TwainSource.GetICapUnits(u, uset);
         if res = crSuccess then
         case u of
               tuInches: begin H := H / 25.4; W := W / 25.4; end;
               tuCentimeters: begin H := H / 10; W := W / 10; end;
//               tuPixels: begin maxw := maxw / xr * 25.4; maxh := maxh / xr * 25.4; end;
          end;
         res := TwainSource.SetImagelayoutFrame(0,0,W,H);
       end;
end;

{ TTwainScanner }

constructor TTwainScanner.Create;
begin
  inherited Create;
  if Assigned(TwainSource)
     then TwainSource.Loaded := true;
  ScannedPix := nil;
end;

destructor TTwainScanner.Destroy;
begin
  TwainSource.Loaded := False;
  inherited Destroy;
end;

function TTwainScanner.ScanToPix: PLPix;
begin
  if TwainSource.Loaded then
     begin
       TwainSource.Enabled := true;
       FScanning := true;
       while FScanning do
             Application.ProcessMessages;
     end;
  Result := ScannedPix;
end;

procedure TTwainScanner.TwainTwainAcquire(Sender: TObject;
  const Index: Integer; Image: TBitmap; var Cancel: Boolean);
var
  Px: PLPix;
  tempimage: TLazIntfImage;
begin
  tempimage := TLazIntfImage.Create(Image.Width, Image.Height);
  tempimage.LoadFromBitmap(Image.Handle, Image.Handle);
  tempimage.SaveToFile('temp.bmp');
  tempimage.Free;
  Px := pixRead('temp.bmp');
  if FileExists('temp.bmp') then DeleteFile('temp.bmp');
  Scannedpix := Px;
  FScanning := false;
  Cancel := true;
end;

end.

