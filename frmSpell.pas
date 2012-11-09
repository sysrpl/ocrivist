unit frmSpell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type
    TSpellResponse = (srChange, srAdd, srIgnore, srCancel);

  { TSpellcheckForm }

  TSpellcheckForm = class ( TForm )
    AddButton: TButton;
    CancelButton: TBitBtn;
    IgnoreButton: TButton;
    ChangeButton: TButton;
    WordEdit: TEdit;
    Label1: TLabel;
    SuggestionList: TListBox;
    procedure AddButtonClick ( Sender: TObject ) ;
    procedure ChangeButtonClick ( Sender: TObject ) ;
    procedure IgnoreButtonClick ( Sender: TObject ) ;
    procedure SuggestionListClick(Sender: TObject);
    procedure SuggestionListDblClick ( Sender: TObject ) ;
    procedure SuggestionListSelectionChange ( Sender: TObject; User: boolean ) ;
  private
    { private declarations }
  public
    { public declarations }
    SpellAction: TSpellResponse;
  end; 

var
  SpellcheckForm: TSpellcheckForm;

implementation

{$R *.lfm}

{ TSpellcheckForm }

procedure TSpellcheckForm.ChangeButtonClick ( Sender: TObject ) ;
begin
  SpellAction := srChange;
  ModalResult := mrOK;
end;

procedure TSpellcheckForm.AddButtonClick ( Sender: TObject ) ;
begin
  SpellAction := srAdd;
  ModalResult := mrOK;
end;

procedure TSpellcheckForm.IgnoreButtonClick ( Sender: TObject ) ;
begin
  SpellAction := srIgnore;
  ModalResult := mrOK;
end;

procedure TSpellcheckForm.SuggestionListClick(Sender: TObject);
begin
  SuggestionListSelectionChange(Sender, false);
end;

procedure TSpellcheckForm.SuggestionListDblClick ( Sender: TObject ) ;
begin
  SuggestionListSelectionChange(Sender, true);
  ChangeButtonClick(Sender);
end;

procedure TSpellcheckForm.SuggestionListSelectionChange ( Sender: TObject;
  User: boolean ) ;
begin
  WordEdit.Text := SuggestionList.Items[SuggestionList.ItemIndex];
end;

end.

