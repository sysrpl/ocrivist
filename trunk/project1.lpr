program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sane, scanutils, MainUI, LibLeptUtils, leptonica, pageviewer,
  OcrivistData, selector, tessintf, DjvuUtils, scanner
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm ( TForm1, Form1 ) ;
  Application.CreateForm ( TScannerForm, ScannerForm ) ;
  ScannerForm.OnChangeScanner := @Form1.UpdateScannerStatus;
  Form1.UpdateScannerStatus(nil);
  Application.Run;
end.

