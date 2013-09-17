unit scanselect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons, StdCtrls, scansane;

type

  { TScannerSelector }

  TScannerSelector = class(TForm)
    CancelButton: TBitBtn;
    NotFoundLabel: TLabel;
    OKButton: TBitBtn;
    DevicesRadioGroup: TRadioGroup;
    ButtonPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    function CheckDevices: integer;
    procedure GetSelectedScannerSettings;
  end;


  function UnderlineToSpace(str: string): string;

var
  ScannerSelector: TScannerSelector;

implementation

uses scanner, progress, Sane;

{$R *.lfm}

function UnderlineToSpace(str: string): string;
var
  p: SizeInt;
begin
  p := Pos('_', str);
  while p>0 do
        begin
          str[p] := #32;
          p := Pos('_', str);
        end;
  Result := str;
end;


{ TScannerSelector }

procedure TScannerSelector.GetSelectedScannerSettings;
var
  DevName: SANE_String_Const;
begin
  DevName := PDevices^[DevicesRadioGroup.ItemIndex]^.name;
  ScannerForm.NameLabel.Caption := UnderlineToSpace(PDevices^[DevicesRadioGroup.ItemIndex]^.model) + #32
                                              + PDevices^[DevicesRadioGroup.ItemIndex]^.dev_type;
  DeviceSelect(DevName);
end;

function TScannerSelector.CheckDevices: integer;
var
  res: SANE_Status;
begin
 ProgressForm.MainTextLabel.Caption := 'Checking for scanners';
 ProgressForm.UpdateTextLabel.Caption := '';
 ProgressForm.Show(nil);
 Application.ProcessMessages;
try
 Result := 0;
 PDevices := nil;
 DevicesRadioGroup.Items.Clear;
 ScannerHandle := nil;
  sane_exit;
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
                 DevicesRadioGroup.Items.Add(UnderlineToSpace(PDevices^[devicecount]^.vendor) + #32
                                              + UnderlineToSpace(PDevices^[devicecount]^.model) + #32
                                              + PDevices^[devicecount]^.dev_type);
                 Inc(devicecount);
               end;
         Application.ProcessMessages;
         if devicecount>0 then
           begin
             DevicesRadioGroup.ItemIndex := 0;
           end;
         OKButton.Enabled := devicecount>0;
         NotFoundLabel.Visible := devicecount=0;
       end;
     end;
 Result := devicecount;
finally
  ProgressForm.Hide;
  Height := DevicesRadioGroup.Top + DevicesRadioGroup.Height + 10 + ButtonPanel.Height;
end;

end;


end.

