program ocrivist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, scanner,
  {$IFDEF HAS_LIBSANE}
  sane, scanutils, scanselect, scansane,
  {$ENDIF}
  {$IFDEF MSWINDOWS} DelphiTwain, scantwain, {$ENDIF}
  MainUI, LibLeptUtils, leptonica, pageviewer, OcrivistData, selector,
  DjvuUtils, ocr, ocreditor, frmSpell, progress, about, historymenu, hpdf,
  tesseract, PasDoc_Aspell
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Ocrivist';
  Application.Initialize;
  Application.CreateForm ( TMainForm, MainForm ) ;
  Application.CreateForm ( TSpellcheckForm, SpellcheckForm ) ;
  Application.CreateForm ( TProgressForm, ProgressForm ) ;
  Application.CreateForm ( TScannerForm, ScannerForm ) ;
  Application.CreateForm ( TAboutForm, AboutForm ) ;
  {$IFDEF HAS_LIBSANE}
  {$ENDIF}
  Application.Run;
end.

