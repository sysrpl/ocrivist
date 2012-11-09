unit progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function ShowModal: Integer; override;
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.lfm}

{ TProgressForm }

function TProgressForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
end;

end.

