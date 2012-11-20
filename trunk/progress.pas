unit progress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TProgressForm }

  TProgressForm = class(TForm)
    CancelButton: TBitBtn;
    ButtonPanel: TPanel;
    MainTextLabel: TLabel;
    UpdateTextLabel: TLabel;
    procedure CancelButtonClick ( Sender: TObject ) ;
    procedure FormCloseQuery ( Sender: TObject; var CanClose: boolean ) ;
  private
    { private declarations }
    FOnCancel: TNotifyEvent;
  public
    { public declarations }
    function ShowModal: Integer; override;
    procedure Show( cancelcb: TNotifyEvent );
    procedure SetMainText( txt: String );
    procedure SetUpdateText( txt: String );
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.lfm}

{ TProgressForm }

procedure TProgressForm.FormCloseQuery ( Sender: TObject; var CanClose: boolean
  ) ;
begin
  CanClose := false;
end;

procedure TProgressForm.CancelButtonClick ( Sender: TObject ) ;
begin
  if Assigned(FOnCancel)
     then FOnCancel(nil);
  ModalResult := mrCancel;
end;

function TProgressForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
end;

procedure TProgressForm.Show ( cancelcb: TNotifyEvent ) ;
begin
  FOnCancel := cancelcb;
  Height := UpdateTextLabel.Top + UpdateTextLabel.Height + MainTextLabel.Top;
  ButtonPanel.Visible := cancelcb<>nil;
  if cancelcb<>nil then Height := Height + ButtonPanel.Height;
  inherited Show;
end;

procedure TProgressForm.SetMainText ( txt: String ) ;
begin
  MainTextLabel.Caption := txt;
  UpdateTextLabel.Caption := '';
end;

procedure TProgressForm.SetUpdateText ( txt: String ) ;
begin
  UpdateTextLabel.Caption := txt;
end;

end.

