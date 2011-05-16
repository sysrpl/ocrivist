unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, Buttons, ComCtrls, OcrivistData, selector, Sane, tessintf;

type

  { TMainForm }

  TMainForm = class ( TForm )
    pagecountLabel: TLabel;
    TesseractButton: TSpeedButton;
    DelSelectButton: TSpeedButton;
    MenuItem1: TMenuItem;
    DeskewMenuItem: TMenuItem;
    RotateRMenuItem: TMenuItem;
    RotateLMenuItem: TMenuItem;
    SetScannerMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    RotateLeftButton: TSpeedButton;
    RotateRightButton: TSpeedButton;
    SelTextButton: TSpeedButton;
    CropButton: TSpeedButton;
    DjvuButton: TSpeedButton;
    DeskewButton: TSpeedButton;
    AnalyseButton: TSpeedButton;
    StatusBar: TStatusBar;
    TestTesseractButton: TButton;
    ThumbnailListBox: TListBox;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    LoadPageMenuItem: TMenuItem;
    DelPageMenuItem: TMenuItem;
    PageMenu: TMenuItem;
    ScanPageMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    SaveProjectDialog: TSaveDialog;
    SaveProjectMenuItem: TMenuItem;
    OpenProjectMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    FitHeightMenuItem: TMenuItem;
    FitWidthMenuItem: TMenuItem;
    View25MenuItem: TMenuItem;
    View100MenuItem: TMenuItem;
    View75MenuItem: TMenuItem;
    View50MenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog: TOpenDialog;
    TopPanel: TPanel;
    ListboxPanel: TPanel;
    RightPanel: TPanel;
    MainPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure CropButtonClick ( Sender: TObject ) ;
    procedure DelPageMenuItemClick ( Sender: TObject ) ;
    procedure DelSelectButtonClick ( Sender: TObject ) ;
    procedure ExitMenuItemClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState
      ) ;
    procedure RotateButtonClick ( Sender: TObject ) ;
    procedure DjvuButtonClick ( Sender: TObject ) ;
    procedure DeskewButtonClick ( Sender: TObject ) ;
    procedure SelTextButtonClick ( Sender: TObject ) ;
    procedure AnalyseButtonClick ( Sender: TObject ) ;
    procedure UpdateScannerStatus ( Sender: TObject ) ;
    procedure ThumbnailListBoxClick ( Sender: TObject ) ;
    procedure ThumbnailListBoxDrawItem ( Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState ) ;
    procedure LoadPageMenuItemClick ( Sender: TObject ) ;
    procedure OpenProjectMenuItemClick ( Sender: TObject ) ;
    procedure SaveAsMenuItemClick ( Sender: TObject ) ;
    procedure ScanPageMenuItemClick ( Sender: TObject ) ;
    procedure SetScannerMenuItemClick ( Sender: TObject ) ;
    procedure TestTesseractButtonClick ( Sender: TObject ) ;
    procedure ViewMenuItemClick ( Sender: TObject ) ;
  private
    { private declarations }
    Project: TOcrivistProject;
    AddingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
    procedure SelectionChange ( Sender: TObject );
    procedure UpdateThumbnail ( Sender: TObject );
    procedure ShowScanProgress(progress: Single);
    procedure LoadPage( newpage: PLPix );
    procedure DoCrop;
  public
    { public declarations }
  end;



const
  THUMBNAIL_HEIGHT = 100;

var
  MainForm: TMainForm;
  ICanvas : TPageViewer;
  Image1:TImage;

 implementation

  uses DjvuUtils, scanner;

  {$R *.lfm}

{ TMainForm }


procedure TMainForm.CropButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smCrop;  writeln('crop button');
  StatusBar.Panels[0].Text := 'CROP';
end;

procedure TMainForm.DelPageMenuItemClick ( Sender: TObject ) ;
var
  ms: TMemoryStream;
  s: Cardinal;
  buf:pbyte;

begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile('/home/malcolm/lazarus/ocrivist2/kscan_0025.bmp');
  s := ms.Size;
  writeln(s);
  Getmem(buf, s);
  ms.Read(buf^, s);
  //writeln(length(buf^));
  ICanvas.Picture := pixReadMem(buf, s);
  ms.Free;
  Freemem(buf, s);
end;

procedure TMainForm.DelSelectButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smDelete;
  StatusBar.Panels[0].Text := 'DELETE';
end;

procedure TMainForm.ExitMenuItemClick ( Sender: TObject ) ;
begin
  Close;
end;

procedure TMainForm.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := MainPanel;
  ICanvas.Align := alClient;
  Icanvas.OnSelect := @MakeSelection;
  ICanvas.OnChangeBitmap := @UpdateThumbnail;
  ThumbnailListBox.Clear;
  ThumbnailListBox.ItemIndex := -1;
  ThumbnailListBox.ItemHeight := THUMBNAIL_HEIGHT + ThumbnailListBox.Canvas.TextHeight('Yy')+2;
  Project := TOcrivistProject.Create;
  Project.Title := 'Default Project';
  AddingThumbnail := false;
end;

procedure TMainForm.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  for x := 0 to ThumbnailListBox.Items.Count-1 do
    TBitmap(ThumbnailListBox.Items.Objects[x]).Free;
  if Assigned(Project)
    then Project.Free;
  if Assigned(ScannerHandle) then
      begin
        sane_close(ScannerHandle);
        sane_exit;
      end;
end;

procedure TMainForm.FormKeyDown ( Sender: TObject; var Key: Word;
  Shift: TShiftState ) ;
  procedure ResetButtons;
  begin
    SelTextButton.Click;
    SelTextButton.Down := true;
  end;
begin
  if CropButton.Down then
     begin
       if Key=27 then begin ResetButtons; ICanvas.ClearSelection; end
       else if Key=13 then  begin DoCrop; ResetButtons; end;
     end;
end;

procedure TMainForm.RotateButtonClick ( Sender: TObject ) ;
var
  newpix: PLPix;
  oldpix: PLPix;
  direction: LongInt;
begin
 direction := 0;
 newpix := nil;
 if Sender is TSpeedButton
    then direction := TSpeedButton(Sender).Tag
 else if Sender is TMenuItem
    then direction := TMenuItem(Sender).Tag;
 if direction<>0 then
   begin
     oldpix := Project.CurrentPage.PageImage;
     newpix := pixRotate90(oldpix, direction);
     if newpix<>nil then
        begin
          if oldpix <> nil then pixDestroy(@oldpix);
          Project.CurrentPage.PageImage := newpix;
          ICanvas.Picture := newpix;
        end;
   end;
end;

procedure TMainForm.DjvuButtonClick ( Sender: TObject ) ;
var
  p: PLPix;
  d: LongInt;
  fn: String;
  x: Integer;
begin
  if SaveProjectDialog.Execute then
     for x := 0 to Project.PageCount-1 do
        try
          p := Project.Pages[x].PageImage;
          d := pixGetDepth(p);
          if d=1
             then fn := '/tmp/test.pnm'
             else fn := '/tmp/test.pbm';
          StatusBar.Panels[1].Text := 'Processing page ' + IntToStr(x+1);
          Application.ProcessMessages;
          pixWrite(PChar(fn), p, IFF_PNM);
          if djvumakepage(fn, '/tmp/test.djvu')=0
             then djvuaddpage(SaveProjectDialog.FileName, '/tmp/test.djvu')
             else ShowMessage('Error when encoding page ' + IntToStr(x+1));
        finally
          StatusBar.Panels[1].Text := '';
        end;
end;

procedure TMainForm.DeskewButtonClick ( Sender: TObject ) ;
var
  oldpix: PLPix;
  newpix: PLPix;
begin
  newpix := nil;
  oldpix := Project.CurrentPage.PageImage;
  newpix := pixDeskew(oldpix, 0);
  if newpix<>nil then
     begin
       ICanvas.Picture := newpix;
       Project.CurrentPage.PageImage := newpix;
      if oldpix<>nil then pixDestroy(@oldpix);
     end;
end;

procedure TMainForm.SelTextButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smSelect;
  StatusBar.Panels[0].Text := 'SELECT';
end;

procedure TMainForm.AnalyseButtonClick ( Sender: TObject ) ;
var
  BinaryPix: PLPix;
  TextMask: PLPix;
  B: PBoxArray;
  x, y, w, h: Integer;
  count: Integer;
  p: PLPix;
begin
  Textmask := nil;
  BinaryPix := nil;
  if pixGetDepth(ICanvas.Picture) > 1
     then BinaryPix := pixThresholdToBinary(ICanvas.Picture, 50)
     else BinaryPix := pixClone(ICanvas.Picture);
  if pixGetRegionsBinary(BinaryPix, nil, nil, @TextMask, 0) = 0 then
     begin
       B := pixConnComp(TextMask, nil, 8);
       try
       boxaWrite('/tmp/boxes.txt', B);
       for count := 0 to boxaGetCount(B)-1 do
            begin
              boxaGetBoxGeometry(B, count, @x, @y, @w, @h);
              ICanvas.AddSelection(Rect(x-10, y-5, x+w+5, y+h{+20}));
            end;
       finally
         if B<>nil then
            boxaDestroy(@B);
         if Textmask <> nil then
            pixDestroy(@TextMask);
       end;
     end;
  if BinaryPix<>nil then
       pixDestroy(@BinaryPix);
end;

procedure TMainForm.UpdateScannerStatus ( Sender: TObject ) ;
begin
  ScanPageMenuItem.Enabled := ScannerHandle<>nil;
  if ScannerHandle=nil then writeln('ScannerHandle is nil')
  else writeln('ScannerHandle ok');;
end;

procedure TMainForm.ThumbnailListBoxClick ( Sender: TObject ) ;
var
  X: Integer;
  S: TSelector;
begin
  if AddingThumbnail then exit;
  for X := Project.CurrentPage.SelectionCount-1 downto 0 do
    ICanvas.DeleteSelector(X);
  Project.ItemIndex := TListBox(Sender).ItemIndex;
  ICanvas.Picture := Project.CurrentPage.PageImage;
  Invalidate;
  for x := 0 to Project.CurrentPage.SelectionCount-1 do
      ICanvas.AddSelection(Project.CurrentPage.Selection[x]);
end;

procedure TMainForm.ThumbnailListBoxDrawItem ( Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState ) ;
var
  LeftOffset: Integer;
begin
  if not assigned(TListbox(Control).Items.Objects[Index]) then exit;
  LeftOffset := (TListbox(Control).ClientWidth-TBitmap(TListbox(Control).Items.Objects[Index]).Width) div 2;
  if LeftOffset<2 then LeftOffset := 2;
  TListbox(Control).Canvas.Rectangle(ARect);
  TListbox(Control).Canvas.Draw(LeftOffset, ARect.Top + TListbox(Control).Canvas.TextHeight('Yy')+2, TBitmap(TListbox(Control).Items.Objects[Index]));
  TListbox(Control).Canvas.TextOut(2, ARect.Top + 2, TListbox(Control).Items[Index]);
  TListbox(Control).Canvas.Pen.Color := clBlack;
  TListbox(Control).Canvas.Frame(ARect);
end;

procedure TMainForm.LoadPageMenuItemClick ( Sender: TObject ) ;
var
  newpage: PLPix;
  pagename: String;
  i: Integer;
begin
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];
  OpenDialog.Filter := 'Image files|*.tif;*.tiff;*.bmp;*.png;*.jpg|All files|*';
  if OpenDialog.Execute then
     for i := 0 to OpenDialog.Files.Count-1 do
        begin
          newpage := pixRead(PChar(OpenDialog.Files[i]));
          if newpage<>nil then
             begin
               pagename :=  ExtractFileNameOnly(OpenDialog.Files[i]);
               pixSetText(newpage, PChar(pagename));
               LoadPage(newpage);
             end
           else ShowMessage('Error when loading page ' + OpenDialog.Files[i]);
        end;
end;

procedure TMainForm.OpenProjectMenuItemClick ( Sender: TObject ) ;
begin
  if OpenDialog.Execute
    then Project.LoadfromFile(OpenDialog.FileName);
end;

procedure TMainForm.SaveAsMenuItemClick ( Sender: TObject ) ;
begin
  if SaveProjectDialog.Execute
    then Project.SaveToFile(SaveProjectDialog.FileName);
end;

procedure TMainForm.ScanPageMenuItemClick ( Sender: TObject ) ;
var
  newpage: PLPix;
  nametext: String;
begin
  if ScannerHandle<>nil then
     begin
       writeln('ScannerHandle OK');
       newpage := ScanToPix(ScannerHandle, ScannerForm.GetResolution, ScannerForm.GetColorMode, nil {@ShowScanProgress});
       nametext := Format('scan_%.4d', [ScannerForm.GetNextCounterValue]);
       newpage^.text := Pchar(nametext);
       if newpage<>nil
        then LoadPage(newpage)
        else ShowMessage('Scan page failed');
     end;
end;

procedure TMainForm.SetScannerMenuItemClick ( Sender: TObject ) ;
begin
 if ScannerHandle=nil then ScannerForm.FormCreate(nil);
 ScannerForm.ShowModal;
end;

procedure TMainForm.TestTesseractButtonClick ( Sender: TObject ) ;
var
  t: Pointer;
  BinaryPix: Pointer;
  pa: PPixArray;
  ba: PBoxArray;
  na: PNumArray;
  x: Integer;
  lineindex: Single;
  linecount: Integer;
  ra : array[0..255] of TRect;
  R: TRect;
  n: Integer;
begin
  t := tesseract_new(nil, nil);
  if t=nil then writeln('tessearct_new failed :(');
//  tesseract_SetImage(t, ICanvas.Picture);
  if pixGetDepth(ICanvas.Picture) > 1
     then BinaryPix := pixThresholdToBinary(ICanvas.Picture, 150)
     else BinaryPix := pixClone(ICanvas.Picture);
 tesseract_SetImage(t, ICanvas.Picture);

 // R := ICanvas.Selection;
 writeln( pixGetWordsInTextlines(BinaryPix, 1, 20, 20, 200, 500, @ba, @pa, @na) );
 writeln('Pixarray.n: ', pa^.n);
 writeln('Pixarray.n: ', pa^.refcount);
 //tesseract_SetPageSegMode(t, PSM_SINGLE_LINE);
 lineindex := na^.numarray[0];
 linecount := 0;
 //writeln(lineindex);
 n := 0;
 while n<ba^.n do
 begin
 Inc(linecount);
 R.Left := 20;
 R.Top := ba^.box[n]^.h + ba^.box[n]^.y;
 while na^.numarray[n]=lineindex do
       begin
         if ba^.box[n]^.y < R.Top then R.Top := ba^.box[n]^.y;
         if ba^.box[n]^.h + ba^.box[n]^.y > R.Bottom then R.Bottom := ba^.box[n]^.h + ba^.box[n]^.y;
         if ba^.box[n]^.x + ba^.box[n]^.w > R.Right then R.Right := ba^.box[n]^.x + ba^.box[n]^.w;
         Inc(n);
         //writeln( FormatFloat('0.00', lineindex));
       end;
 x := R.Bottom-R.Top;
 x := 0;
 R.Top := R.Top-x;
 R.Bottom := R.Bottom+x;
 //ICanvas.AddSelection(R);
 x := trunc(lineindex);
 ra[x] := R;
 lineindex := na^.numarray[n];
 writeln( FormatFloat('0.00', lineindex));
 end;

 writeln( FormatFloat('0.00', lineindex));
// n := Trunc(lineindex)-1;
 writeln(linecount);
 for x := 0 to linecount-1 do
      begin
        R := ra[x];
        tesseract_SetRectangle(t, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top);
        ICanvas.AddSelection(R);
        Application.ProcessMessages;
        WriteLn(tesseract_GetUTF8Text(t));
      end;


 {for x := 0 to pa^.n-1 do
      begin
        //pixWrite(Pchar('/tmp/test'+IntToStr(x) +'.bmp'), pa^.pix[x], IFF_BMP);
        //tesseract_SetImage(t, pa^.pix[x]);
        //tesseract_SetRectangle(t, ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w, ba^.box[x]^.h);
        ICanvas.AddSelection( Rect(ba^.box[x]^.x, ba^.box[x]^.y, ba^.box[x]^.w + ba^.box[x]^.x, ba^.box[x]^.h + ba^.box[x]^.y) );
        //Write(tesseract_GetUTF8Text(t), #32);
      end;  }


  //tesseract_GetUTF8Text(t);
  //WriteLn(tesseract_GetBoxText(t, 0));
  pixDestroy(@BinaryPix);
  tesseract_destroy(t);

end;

procedure TMainForm.ViewMenuItemClick ( Sender: TObject ) ;
begin
  Case TMenuItem(Sender).Tag of
       0: ICanvas.Mode := vmFitToHeight;   // Fit to Height
       1: ICanvas.Mode := vmFitToWidth;    // Fit to Width
       else
         ICanvas.Scale := TMenuItem(Sender).Tag;
       end;
end;

procedure TMainForm.MakeSelection ( Sender: TObject ) ;
var
  S: TSelector;
begin
  if ICanvas.SelectionMode<>smCrop then
    begin
      ICanvas.AddSelection(UnScaleRect(ICanvas.Selection, ICanvas.Scale));
      Project.CurrentPage.AddSelection( UnScaleRect (ICanvas.Selection, ICanvas.Scale) )
    end;
end;

procedure TMainForm.SelectionChange ( Sender: TObject ) ;
var
  SelectionId: LongInt;
begin
  SelectionId := StrToInt( TSelector(Sender).Caption )-1;
  Project.CurrentPage.Selection[SelectionId] := UnScaleRect(TSelector(Sender).Selection, ICanvas.Scale);
end;

procedure TMainForm.UpdateThumbnail ( Sender: TObject ) ;
var
  thumbBMP: TBitmap;
begin
 if AddingThumbnail then exit
    else if ThumbnailListBox.Count<1 then exit;
 thumbBMP := ICanvas.GetThumbnail(THUMBNAIL_HEIGHT, THUMBNAIL_HEIGHT);
 if thumbBMP<>nil then
   begin
     TBitmap(ThumbnailListBox.Items.Objects[ThumbnailListBox.ItemIndex]).Free;
     ThumbnailListBox.Items.Objects[ThumbnailListBox.ItemIndex] := thumbBMP;
     ThumbnailListBox.Invalidate;
   end;
end;

procedure TMainForm.ShowScanProgress ( progress: Single ) ;
begin
  writeln( FormatFloat('0.00', progress));
end;

procedure TMainForm.LoadPage ( newpage: PLPix ) ;
var
  X: Integer;
  thumbBMP: TBitmap;
begin
 AddingThumbnail := true;  //avoid triggering reload through ThumbList.Click;
 ICanvas.Picture := newpage;
 Application.ProcessMessages;  // Draw the screen before doing some background work
 If Project.ItemIndex>=0
  then for X := Project.CurrentPage.SelectionCount-1 downto 0 do
       ICanvas.DeleteSelector(X);
 Project.AddPage(ICanvas.Picture);
 thumbBMP := ICanvas.GetThumbnail(THUMBNAIL_HEIGHT, THUMBNAIL_HEIGHT);
 if thumbBMP<>nil then
   begin
     ThumbnailListBox.Items.AddObject(newpage^.text, thumbBMP);
   end;
 ThumbnailListBox.ItemIndex := ThumbnailListBox.Count-1;
 if ThumbnailListBox.Count>1
  then pagecountLabel.Caption := Format('%d pages', [ThumbnailListBox.Count])
  else pagecountLabel.Caption := #32;  // if label is empty it changes its height
 AddingThumbnail := false;
end;

procedure TMainForm.DoCrop;
var
  newpix: PLPix;
  oldpix: PLPix;
begin
 newpix := nil;
 oldpix := ICanvas.Picture;
 newpix := CropPix( oldpix, ScaleRect( ICanvas.Selection , 100/ICanvas.Scale));
 if newpix <> nil then
   begin
     ICanvas.ClearSelection;
     ICanvas.Picture := newpix;
     Project.CurrentPage.PageImage := newpix;
     if oldpix <> nil
        then pixDestroy(@oldpix);
   end;
end;


end.

