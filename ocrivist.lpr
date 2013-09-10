program ocrivist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  {$IFDEF HAS_LIBSANE}
  sane, scanutils, scanner, scanselect,
  {$ENDIF}
  MainUI, LibLeptUtils, leptonica, pageviewer, OcrivistData, selector,
  DjvuUtils, ocr, ocreditor, frmSpell, progress, about, threshold, tesseract,
  PasDoc_Aspell
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Ocrivist';
  Application.Initialize;
  Application.CreateForm ( TMainForm, MainForm ) ;
  Application.CreateForm ( TSpellcheckForm, SpellcheckForm ) ;
  Application.CreateForm ( TProgressForm, ProgressForm ) ;
  {$IFDEF HAS_LIBSANE}
  Application.CreateForm ( TScannerForm, ScannerForm ) ;
  Application.CreateForm ( TScannerSelector, ScannerSelector ) ;
  {$ENDIF}
  Application.CreateForm ( TAboutForm, AboutForm ) ;
  Application.Run;
end.

