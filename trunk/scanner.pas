unit scanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, Buttons, Sane;

type

  { TScannerForm }

  TScannerForm = class ( TForm )
    OKButton: TBitBtn;
    ModeComboBox: TComboBox;
    DeviceComboBox: TComboBox;
    ResolutionSpinEdit: TSpinEdit;
    procedure FormCreate ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ScannerForm: TScannerForm;
  ScannerHandle: SANE_Handle;
  Devices: DeviceArray;
  devicecount: Integer;

implementation

{$R *.lfm}

{ TScannerForm }

procedure TScannerForm.FormCreate ( Sender: TObject ) ;
begin
  ScannerHandle := nil;
  if sane_init(nil, nil) = SANE_STATUS_GOOD then
     if sane_get_devices ( @devices, SANE_TRUE) = SANE_STATUS_GOOD then
     begin
       devicecount := 0;
       while Devices[devicecount] <> nil do
             begin
               DeviceComboBox.Items.Add(Devices[devicecount]^.model);
               Inc(devicecount);
             end;
       Application.ProcessMessages;
       if devicecount>0 then
         begin
           DeviceComboBox.ItemIndex := 0;
           sane_open(PChar(Devices[0]^.name), @ScannerHandle);
         end;
     end;

end;

end.

