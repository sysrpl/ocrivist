unit uFormSelectSource;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls;

type
  TFormSelectSource = class(TForm)
    LBSources: TListBox;
    PnlBottom: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure PnlBottomResize(Sender: TObject);
    procedure LBSourcesDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFormSelectSource.LBSourcesDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormSelectSource.PnlBottomResize(Sender: TObject);
begin
  BtnCancel.Left := PnlBottom.ClientWidth - BtnCancel.Width - 6;
  BtnOK.Left := BtnCancel.Left - BtnOK.Width - 6;
end;

end.
