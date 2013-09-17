unit scansane;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, scanner, sane, ScanUtils;

procedure DeviceSelect(DeviceName: string);
procedure InitSource( saneoption: SANE_Option_Descriptor );
procedure InitResolution( saneoption: SANE_Option_Descriptor );
procedure InitMode( mode: SANE_Option_Descriptor );
function SetScannerOptions: integer;
function SelectScanner: Boolean;


implementation

uses scanselect;

procedure DeviceSelect(DeviceName: string);
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
                                                     result := round( SANE_UNFIX( range^.max));
                                                  end;
                                            end;
                                    end;
    end;
  end;
begin
     begin
       writeln('getting Devname');
       //DevName := PDevices^[DeviceComboBox.ItemIndex]^.name;
       DevName := PChar(DeviceName);
       writeln(DevName, ' open request');
       if sane_open(PChar(DevName), @ScannerHandle) = SANE_STATUS_GOOD then
       with ScannerForm do
          begin
            writeln(DevName, ' open');
            InitSource(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_SOURCE));
            InitResolution(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_RESOLUTION));
            InitMode(SaneGetOption(ScannerHandle, SANE_NAME_SCAN_MODE));
            option := SaneGetOption(ScannerHandle, SANE_NAME_SCAN_BR_X);
            WidthSpinEdit.MaxValue := GetMaxValue(option);
            option := SaneGetOption(ScannerHandle, SANE_NAME_SCAN_BR_Y);
            HeightSpinEdit.MaxValue := GetMaxValue(option);
          end;
       ScannerForm.ResolutionComboBox.ItemIndex := ScannerForm.ResolutionComboBox.Items.IndexOf('300');
       ScannerForm.PaperFormatBoxChange(nil);
     end;
end;

procedure InitSource(saneoption: SANE_Option_Descriptor);
type
 strlist = array[0..255] of PChar;
 pstrlist = ^strlist;
var
  str: pstrlist;
  RangeIndex: Integer;
begin
  with ScannerForm do
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
end;

procedure InitResolution(saneoption: SANE_Option_Descriptor);
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
  with ScannerForm do
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
                                           if (rangemin<=75) and (rangemax>=75) then ResolutionComboBox.Items.Add('75');
                                           if (rangemin<=150) and (rangemax>=150) then ResolutionComboBox.Items.Add('150');
                                           if (rangemin<=300) and (rangemax>=300) then ResolutionComboBox.Items.Add('300');
                                           if (rangemin<=400) and (rangemax>=400) then ResolutionComboBox.Items.Add('400');
                                           if (rangemin<=600) and (rangemax>=600) then ResolutionComboBox.Items.Add('600');
                                           if (rangemin<=800) and (rangemax>=800) then ResolutionComboBox.Items.Add('800');
                                           if (rangemin<=1200) and (rangemax>=1200) then ResolutionComboBox.Items.Add('1200');
                                        end;
            SANE_CONSTRAINT_WORD_LIST:  begin
                                           ResolutionComboBox.Items.Clear;
                                           w := pwordlist(saneoption.pwordlist);
    //                                       ResolutionComboBox.Items.Add(IntToStr(w^[0]) + ' items in list:');
                                           if saneoption.option_type=SANE_TYPE_INT then
                                             for x := 1 to w^[0] do
                                                ResolutionComboBox.Items.Add(IntToStr(w^[x]))
                                           else
                                           if saneoption.option_type=SANE_TYPE_FIXED then
                                             for x := 1 to w^[0] do
                                                ResolutionComboBox.Items.Add(FloatToStr( SANE_UNFIX(w^[x]) ) );
                                        end;
       end;
       ResolutionComboBox.Enabled := ResolutionComboBox.Items.Count>0;
       if ResolutionComboBox.Enabled then ResolutionComboBox.ItemIndex := ResolutionComboBox.Items.IndexOf('300') else ResolutionComboBox.ItemIndex := -1;

    end;
end;

procedure InitMode(mode: SANE_Option_Descriptor);
type
 strlist = array[0..255] of PChar;
 pstrlist = ^strlist;
var
  str: pstrlist;
  RangeIndex: Integer;
begin
  with ScannerForm do
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
end;

function SetScannerOptions: integer;
begin
  Result := -1;
  if ScannerHandle=nil then exit;
  try
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_RESOLUTION, IntToStr(ScannerForm.GetResolution) );
    writeln('resolution set');
     // set selected scan mode
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_MODE, ScannerForm.GetColorMode) ;
     writeln('mode set');
     // set selected source
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_SOURCE, ScannerForm.GetSource) ;
     writeln('source set');
     // set page bottomright
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_BR_X, IntToStr(ScannerForm.WidthSpinEdit.Value)) ;
     SaneSetOption(ScannerHandle, SANE_NAME_SCAN_BR_Y, IntToStr(ScannerForm.HeightSpinEdit.Value)) ;
     writeln('page size set');
     Result := 0;
  except
    ScannerHandle := nil;
  end;
end;

function SelectScanner: Boolean;
begin
  ScannerSelector := TScannerSelector.Create(nil);
  try
    Result := ScannerSelector.CheckDevices>0;
    if ScannerSelector.ShowModal=mrOK then
       begin
//         Application.ProcessMessages;
         ScannerSelector.GetSelectedScannerSettings;
         ScannerForm.ShowModal;
       end
    else Result := false;
  finally
    ScannerSelector.Free;
  end;

end;

end.
