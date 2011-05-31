program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sane, scanutils, MainUI, LibLeptUtils, leptonica, pageviewer,
  OcrivistData, selector, tessintf, DjvuUtils, scanner, ocr
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'Ocrivist';
  Application.Initialize;
  Application.CreateForm ( TMainForm, MainForm ) ;
  Application.CreateForm ( TScannerForm, ScannerForm ) ;
  ScannerForm.OnChangeScanner := @MainForm.UpdateScannerStatus;
  MainForm.UpdateScannerStatus(nil);
  Application.Run;
end.

