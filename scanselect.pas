unit scanselect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, Buttons;

type

  { TScannerSelector }

  TScannerSelector = class(TForm)
    CancelButton: TBitBtn;
    OKButton: TBitBtn;
    DevicesRadioGroup: TRadioGroup;
    ButtonPanel: TPanel;
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function CheckDevices: integer;
  end;

var
  ScannerSelector: TScannerSelector;

implementation

uses scanner, progress, Sane;

{$R *.lfm}

{ TScannerSelector }

procedure TScannerSelector.OKButtonClick(Sender: TObject);
var
  DevName: SANE_String_Const;
begin
  DevName := PDevices^[DevicesRadioGroup.ItemIndex]^.name;
  ScannerForm.NameLabel.Caption := PDevices^[DevicesRadioGroup.ItemIndex]^.model;
  ScannerForm.DeviceSelect(DevName);
  ModalResult := mrOK;
end;

function TScannerSelector.CheckDevices: integer;
var
  res: SANE_Status;
begin
 ProgressForm.Label1.Caption := 'Checking for scanners';
 ProgressForm.Label2.Caption := '';
 ProgressForm.Show(false);
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
                 DevicesRadioGroup.Items.Add(PDevices^[devicecount]^.model);
                 Inc(devicecount);
               end;
         Application.ProcessMessages;
         if devicecount>0 then
           begin
             DevicesRadioGroup.ItemIndex := 0;
           end;
         OKButton.Enabled := devicecount>0;
       end;
     end;
 Result := devicecount;
finally
  ProgressForm.Hide;
end;

end;


end.

