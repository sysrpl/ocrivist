unit scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$IFDEF HAS_LIBSANE}
  Sane,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DelphiTwain,
  {$ENDIF}
  ComCtrls, Spin, Buttons;

type

  { TScannerForm }

  TScannerForm = class ( TForm )
    Label9: TLabel;
    NameLabel: TLabel;
    SourceComboBox: TComboBox;
    Label8: TLabel;
    PaperFormatBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ModeComboBox: TComboBox;
    OKButton: TBitBtn;
    PageControl1: TPageControl;
    ResolutionComboBox: TComboBox;
    ScanSheet1: TTabSheet;
    ScanSheet2: TTabSheet;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    procedure HeightSpinEditChange ( Sender: TObject ) ;
    procedure PaperFormatBoxChange ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure WidthSpinEditChange ( Sender: TObject ) ;
  private
    { private declarations }
    MaxPageHeight: Integer;
    MaxPageWidth: Integer;
  public
    { public declarations }
    OnChangeScanner: TNotifyEvent;
    function GetColorMode: string;
    function GetSource: string;
    function GetResolution: Integer;
    procedure InitialiseResolution(minval, maxval, current: Integer);
  end;

  TPaperformats =(pfCustom, pfA3, pfA4, pfA5, pfB4, pfB5, pfB6, pfLetter, pfExecutive);

var
  ScannerForm: TScannerForm;
  {$IFDEF HAS_LIBSANE}
  ScannerHandle: SANE_Handle;
  PDevices: PDeviceArray;
  {$ENDIF}
  devicecount: Integer;
  CurrentScanner: string;

implementation

uses {$IFDEF HAS_LIBSANE} ScanUtils,{$ENDIF} progress;

{$R *.lfm}

{ TScannerForm }

procedure TScannerForm.FormCreate ( Sender: TObject ) ;
begin
  //CheckDevices;
end;

procedure TScannerForm.WidthSpinEditChange ( Sender: TObject ) ;
begin
  case WidthSpinEdit.Value of
       125: if HeightSpinEdit.Value=176 then PaperFormatBox.ItemIndex := Integer( pfB6 );
       148: if HeightSpinEdit.Value=210 then PaperFormatBox.ItemIndex := Integer( pfA5 );
       176: if HeightSpinEdit.Value=250 then PaperFormatBox.ItemIndex := Integer( pfB5 );
       190: if HeightSpinEdit.Value=254 then PaperFormatBox.ItemIndex := Integer( pfExecutive );
       216: if HeightSpinEdit.Value=279 then PaperFormatBox.ItemIndex := Integer( pfLetter );
       210: if HeightSpinEdit.Value=297 then PaperFormatBox.ItemIndex := Integer( pfA4 );
       250: if HeightSpinEdit.Value=353 then PaperFormatBox.ItemIndex := Integer( pfB4 );
       297: if HeightSpinEdit.Value=420 then PaperFormatBox.ItemIndex := Integer( pfA3 );
       else  PaperFormatBox.ItemIndex := 0;
  end;
end;

function TScannerForm.GetColorMode: string;
begin
  Result := '';
  if ModeComboBox.ItemIndex>-1
     then Result := ModeComboBox.Items[ModeComboBox.ItemIndex];
end;

function TScannerForm.GetSource: string;
begin
 Result := '';
 if SourceComboBox.ItemIndex>-1
    then Result := SourceComboBox.Items[SourceComboBox.ItemIndex];
end;

function TScannerForm.GetResolution: Integer;
begin
  Result := 300;
  if ResolutionComboBox.ItemIndex>-1
     then Result := StrToInt(ResolutionComboBox.Items[ResolutionComboBox.ItemIndex]);
end;

procedure TScannerForm.InitialiseResolution(minval, maxval, current: Integer);
var
  i: Integer;
begin
  ResolutionComboBox.Items.Clear;
  if (minval<=75) and (maxval>=75) then ResolutionComboBox.Items.Add('75');
  if (minval<=150) and (maxval>=150) then ResolutionComboBox.Items.Add('150');
  if (minval<=300) and (maxval>=300) then ResolutionComboBox.Items.Add('300');
  if (minval<=400) and (maxval>=400) then ResolutionComboBox.Items.Add('400');
  if (minval<=600) and (maxval>=600) then ResolutionComboBox.Items.Add('600');
  if (minval<=800) and (maxval>=800) then ResolutionComboBox.Items.Add('800');
  if (minval<=1200) and (maxval>=1200) then ResolutionComboBox.Items.Add('1200');
  i := ResolutionComboBox.Items.IndexOf(IntToStr(current));
  if i>-1
     then ResolutionComboBox.ItemIndex := i
     else ResolutionComboBox.ItemIndex := ResolutionComboBox.Items.IndexOf('300');
end;

procedure TScannerForm.PaperFormatBoxChange ( Sender: TObject ) ;
var
  papersize: TPaperformats;
begin
 papersize := TPaperformats(PaperFormatBox.ItemIndex);
 Case papersize of
       pfA3:   begin WidthSpinEdit.Value := 297; HeightSpinEdit.Value := 420; end;
       pfA4:   begin WidthSpinEdit.Value := 210; HeightSpinEdit.Value := 297; end;
       pfA5:   begin WidthSpinEdit.Value := 148; HeightSpinEdit.Value := 210; end;
       pfB4:   begin WidthSpinEdit.Value := 250; HeightSpinEdit.Value := 353; end;
       pfB5:   begin WidthSpinEdit.Value := 176; HeightSpinEdit.Value := 250; end;
       pfB6:   begin WidthSpinEdit.Value := 125; HeightSpinEdit.Value := 176; end;
       pfLetter:   begin WidthSpinEdit.Value := 216; HeightSpinEdit.Value := 279; end;
       pfExecutive:   begin WidthSpinEdit.Value := 190; HeightSpinEdit.Value := 254; end;
  end;
end;

procedure TScannerForm.HeightSpinEditChange ( Sender: TObject ) ;
begin
case HeightSpinEdit.Value of
     176: if WidthSpinEdit.Value=125 then PaperFormatBox.ItemIndex := Integer( pfB6 );
     210: if WidthSpinEdit.Value=148 then PaperFormatBox.ItemIndex := Integer( pfA5 );
     250: if WidthSpinEdit.Value=176 then PaperFormatBox.ItemIndex := Integer( pfB5 );
     254: if WidthSpinEdit.Value=190 then PaperFormatBox.ItemIndex := Integer( pfExecutive );
     279: if WidthSpinEdit.Value=216 then PaperFormatBox.ItemIndex := Integer( pfLetter );
     297: if WidthSpinEdit.Value=210 then PaperFormatBox.ItemIndex := Integer( pfA4 );
     353: if WidthSpinEdit.Value=250 then PaperFormatBox.ItemIndex := Integer( pfB4 );
     420: if WidthSpinEdit.Value=297 then PaperFormatBox.ItemIndex := Integer( pfA3 );
     else  PaperFormatBox.ItemIndex := 0;
end;

end;

end.

