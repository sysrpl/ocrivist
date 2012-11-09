unit scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, Buttons, Sane;

type

  { TScannerForm }

  TScannerForm = class ( TForm )
    SourceComboBox: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    PaperFormatBox: TComboBox;
    DeviceComboBox: TComboBox;
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
    CounterSpinEdit: TSpinEdit;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    procedure PaperFormatBoxChange ( Sender: TObject ) ;
    procedure DeviceComboBoxChange ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure PaperSideEditChange ( Sender: TObject ) ;
  private
    { private declarations }
    MaxPageHeight: Integer;
    MaxPageWidth: Integer;
  public
    { public declarations }
    OnChangeScanner: TNotifyEvent;
    procedure InitSource( saneoption: SANE_Option_Descriptor );
    procedure InitResolution( saneoption: SANE_Option_Descriptor );
    procedure InitMode( mode: SANE_Option_Descriptor );
    function SetScannerOptions: integer;
    function CheckDevices: Integer;
    function GetColorMode: string;
    function GetSource: string;
    function GetResolution: Integer;
    function GetNextCounterValue: Integer;
  end;

  TPaperformats =(pfCustom, pfA3, pfA4, pfA5, pfB4, pfB5, pfB6, pfLetter, pfExecutive);

var
  ScannerForm: TScannerForm;
  ScannerHandle: SANE_Handle;
  PDevices: PDeviceArray;
  devicecount: Integer;

implementation

uses ScanUtils, progress;

{$R *.lfm}

{ TScannerForm }

procedure TScannerForm.FormCreate ( Sender: TObject ) ;
begin
  //CheckDevices;
end;

procedure TScannerForm.PaperSideEditChange ( Sender: TObject ) ;
begin
  PaperFormatBox.ItemIndex := 0;
end;

procedure TScannerForm.InitSource(saneoption: SANE_Option_Descriptor);
type
 strlist = array[0..255] of PChar;
 pstrlist = ^strlist;
var
  str: pstrlist;
  RangeIndex: Integer;
begin
  SourceComboBox.Items.Clear;
  if saneoption.name=SANE_NAME_SCAN_SOURCE then
  if saneoption.constraint_type = SANE_CONSTRAINT_STRING_LIST then
    begin
       str := pstrlist(saneoption.pstringlist);
       RangeIndex := 0;
       while str^[RangeIndex] <> nil do
             begin
               SourceComboBox.Items.Add(str^[RangeIndex]);
               Inc(RangeIndex);
             end;
       SourceComboBox.Enabled := SourceComboBox.Items.Count>0;
    end;
  if SourceComboBox.Enabled then SourceComboBox.ItemIndex := 0 else SourceComboBox.ItemIndex := -1;
end;

procedure TScannerForm.InitResolution ( saneoption: SANE_Option_Descriptor ) ;
type
 wordlist = array [0..128] of SANE_Word;
 pwordlist = ^wordlist;
var
  w: pwordlist;
  x: Integer;
  rangemin: Int64;
  rangemax: Int64;
  range: pSANE_Range;
begin
  ResolutionComboBox.Items.Clear;
  if saneoption.name=SANE_NAME_SCAN_RESOLUTION then
   case saneoption.constraint_type of
        SANE_CONSTRAINT_RANGE:      begin
                                       range := saneoption.prange;
                                       case saneoption.option_type of
                                            SANE_TYPE_INT :
                                                  begin
                                                     rangemin := range^.min;
                                                     rangemax := range^.max;
                                                  end;
                                            SANE_TYPE_FIXED :
                                                  begin
                                                     rangemin := Trunc( SANE_UNFIX( range^.min));
                                                     rangemax := Trunc( SANE_UNFIX( range^.max));
                                                  end;
                                            end;
                                       if (rangemin<=150) and (rangemax>=150) then ResolutionComboBox.Items.Add('150');
                                       if (rangemin<=300) and (rangemax>=300) then ResolutionComboBox.Items.Add('300');
                                       if (rangemin<=600) and (rangemax>=600) then ResolutionComboBox.Items.Add('600');
                                       if (rangemin<=1200) and (rangemax>=1200) then ResolutionComboBox.Items.Add('1200');
                                    end;
        SANE_CONSTRAINT_WORD_LIST:  begin
                                       ResolutionComboBox.Items.Clear;
                                       w := pwordlist(saneoption.pwordlist);
//                                       ResolutionComboBox.Items.Add(IntToStr(w^[0]) + ' items in list:');
                                       if saneoption.option_type=SANE_TYPE_INT then
                                         for x := 1 to w^[0] do
                                            ResolutionComboBox.Items.Add('    ' + IntToStr(w^[x]))
                                       else
                                       if saneoption.option_type=SANE_TYPE_FIXED then
                                         for x := 1 to w^[0] do
                                            ResolutionComboBox.Items.Add('    ' + FloatToStr( SANE_UNFIX(w^[x]) ) );
                                    end;
   end;
   ResolutionComboBox.Enabled := ResolutionComboBox.Items.Count>0;
   if ResolutionComboBox.Enabled then ResolutionComboBox.ItemIndex := ResolutionComboBox.Items.IndexOf('300') else ResolutionComboBox.ItemIndex := -1;
end;

procedure TScannerForm.InitMode ( mode: SANE_Option_Descriptor ) ;
type
 strlist = array[0..255] of PChar;
 pstrlist = ^strlist;
var
  str: pstrlist;
  RangeIndex: Integer;
begin
  ModeComboBox.Items.Clear;
  if mode.name=SANE_NAME_SCAN_MODE then
  if mode.constraint_type = SANE_CONSTRAINT_STRING_LIST then
    begin
       str := pstrlist(mode.pstringlist);
       RangeIndex := 0;
       while str^[RangeIndex] <> nil do
             begin
               ModeComboBox.Items.Add(str^[RangeIndex]);
               Inc(RangeIndex);
             end;
       ModeComboBox.Enabled := ModeComboBox.Items.Count>0;
    end;
  if ModeComboBox.Enabled then ModeComboBox.ItemIndex := 0 else ModeComboBox.ItemIndex := -1;
end;

function TScannerForm.SetScannerOptions: integer;
begin
  Result := -1;
  if ScannerHandle=nil then exit;
  try
    writeln('open');
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_RESOLUTION, IntToStr(GetResolution) );
    writeln('resolution set');
     // set selected scan mode
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_MODE, GetColorMode) ;
     writeln('mode set');
     // set selected source
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_SOURCE, GetSource) ;
     writeln('source set');
     Result := 0;
  except
    ScannerHandle := nil;
  end;
end;

function TScannerForm.CheckDevices: Integer;
var
  res: SANE_Status;
begin
 ProgressForm.Label1.Caption := 'Checking for scanners';
 ProgressForm.Label2.Caption := '';
 ProgressForm.Show;
 Application.ProcessMessages;
try
 Result := 0;
 PDevices := nil;
  ScannerHandle := nil;
  res := sane_init(nil, nil);
  if res = SANE_STATUS_GOOD then
     begin
       Getmem(PDevices, sizeof(PDevices));
       res := sane_get_devices ( PDevices, SANE_TRUE);
       if res = SANE_STATUS_GOOD then
       begin
         //Devices := PDevicelist^;
         devicecount := 0;
         while PDevices^[devicecount] <> nil do
               begin
                 DeviceComboBox.Items.Add(PDevices^[devicecount]^.model);
                 Inc(devicecount);
               end;
         Application.ProcessMessages;
         if devicecount>0 then
           begin
             DeviceComboBox.ItemIndex := 0;
             DeviceComboBoxChange(nil);
           end;
       end;
     end;
 PaperFormatBoxChange(nil);
 Result := devicecount;

finally
  ProgressForm.Hide;
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

function TScannerForm.GetNextCounterValue: Integer;
begin
 if CounterSpinEdit.Value=CounterSpinEdit.MaxValue
    then CounterSpinEdit.Value := CounterSpinEdit.MinValue
    else CounterSpinEdit.Value := CounterSpinEdit.Value+1;
 Result := CounterSpinEdit.Value;
end;

procedure TScannerForm.DeviceComboBoxChange ( Sender: TObject ) ;
var
  scanmode: SANE_Option_Descriptor;
  scanres: SANE_Option_Descriptor;
  DevName: PChar;
  option: SANE_Option_Descriptor;

  function GetMaxValue(opt: SANE_Option_Descriptor): Integer;
  var
    range: pSANE_Range;
  begin
    Result := 0;
    case opt.constraint_type of
        SANE_CONSTRAINT_RANGE:      begin
                                       range := opt.prange;
                                       case opt.option_type of
                                            SANE_TYPE_INT :
                                                  begin
                                                     result := range^.max;
                                                  end;
                                            SANE_TYPE_FIXED :
                                                  begin
                                                     result := Trunc( SANE_UNFIX( range^.max));
                                                  end;
                                            end;
                                    end;
    end;
  end;
begin
  if ScannerHandle<>nil then sane_close(ScannerHandle) ;
  if DeviceComboBox.ItemIndex>-1 then
     begin
       DevName := PDevices^[DeviceComboBox.ItemIndex]^.name;
       if sane_open(PChar(DevName), @ScannerHandle) = SANE_STATUS_GOOD then
          begin
            InitSource(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_SOURCE));
            InitResolution(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_RESOLUTION));
            InitMode(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_MODE));
            option := SaneGetOption(ScannerHandle, SANE_NAME_SCAN_BR_X);
            WidthSpinEdit.MaxValue := GetMaxValue(option);
            option := SaneGetOption(ScannerHandle, SANE_NAME_SCAN_BR_Y);
            HeightSpinEdit.MaxValue := GetMaxValue(option);
          end;
     end;
 if Assigned(OnChangeScanner) then OnChangeScanner(Self);
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

end.

