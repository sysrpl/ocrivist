program ocrivist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sane, scanutils, MainUI, LibLeptUtils, leptonica, pageviewer,
  OcrivistData, selector, tessintf, DjvuUtils, scanner, ocr, ocreditor,
  frmSpell, progress, scanselect, about
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Ocrivist';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSpellcheckForm, SpellcheckForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TScannerForm, ScannerForm);
  Application.CreateForm(TScannerSelector, ScannerSelector);
  Application.CreateForm ( TAboutForm, AboutForm ) ;
  Application.Run;
end.

