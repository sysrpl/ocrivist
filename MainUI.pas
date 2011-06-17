unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, Buttons, ComCtrls, SynMemo, SynEdit, SynHighlighterAny,
  OcrivistData, selector, Sane;

type

  { TMainForm }

  TMainForm = class ( TForm )
    ImageList1: TImageList;
    LoadModeMenu: TPopupMenu;
    LoadModeFileButton: TMenuItem;
    LoadModeScanButton: TMenuItem;
    ExportModeMenu: TPopupMenu;
    ExportDjvuButton: TMenuItem;
    ExportTextButton: TMenuItem;
    ExportPDFButton: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SelModeSelectButton: TMenuItem;
    SelModeDeleteButton: TMenuItem;
    SelModeCropButton: TMenuItem;
    pagecountLabel: TLabel;
    SelModeMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    RotateRMenuItem: TMenuItem;
    RotateLMenuItem: TMenuItem;
    SetScannerMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    StatusBar: TStatusBar;
    SynAnySyn1: TSynAnySyn;
    SynMemo1: TSynMemo;
    ThumbnailListBox: TListBox;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    LoadPageMenuItem: TMenuItem;
    DelPageMenuItem: TMenuItem;
    PageMenu: TMenuItem;
    ScanPageMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    SaveDialog: TSaveDialog;
    SaveProjectMenuItem: TMenuItem;
    OpenProjectMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    FitHeightMenuItem: TMenuItem;
    FitWidthMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    RotateLeftTButton: TToolButton;
    FitHeightButton: TToolButton;
    AddPageButton: TToolButton;
    ToolButton1: TToolButton;
    RotateRightButton: TToolButton;
    SelectToolButton: TToolButton;
    TesseractButton: TToolButton;
    FitWidthButton: TToolButton;
    SaveButton: TToolButton;
    AutoselectButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    DelPageButton: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ExportButton: TToolButton;
    DeskewButton: TToolButton;
    View25MenuItem: TMenuItem;
    View100MenuItem: TMenuItem;
    View75MenuItem: TMenuItem;
    View50MenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog: TOpenDialog;
    ListboxPanel: TPanel;
    RightPanel: TPanel;
    MainPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure CropButtonClick ( Sender: TObject ) ;
    procedure DelPageMenuItemClick ( Sender: TObject ) ;
    procedure DelSelectButtonClick ( Sender: TObject ) ;
    procedure ExitMenuItemClick ( Sender: TObject ) ;
    procedure ExportModeMenuClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState
      ) ;
    procedure LoadModeOptionClick(Sender: TObject);
    procedure RotateButtonClick ( Sender: TObject ) ;
    procedure DjvuButtonClick ( Sender: TObject ) ;
    procedure DeskewButtonClick ( Sender: TObject ) ;
    procedure SelTextButtonClick ( Sender: TObject ) ;
    procedure AnalyseButtonClick ( Sender: TObject ) ;
    procedure ThumbnailListBoxDragDrop ( Sender, Source: TObject; X, Y: Integer
      ) ;
    procedure ThumbnailListBoxDragOver ( Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean ) ;
    procedure ThumbnailListBoxMouseDown ( Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
    procedure ThumbnailListBoxMouseUp ( Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer ) ;
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
    ThumbnailStartPoint: TPoint;
    DraggingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
    procedure SelectionChange ( Sender: TObject );
    procedure UpdateThumbnail ( Sender: TObject );
    procedure ShowScanProgress( progress: Single );
    procedure ShowOCRProgress ( progress: Single );
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
  OCRProgressCount: integer;

 implementation

  uses DjvuUtils, scanner, ocr;

  {$R *.lfm}

{ TMainForm }


procedure TMainForm.CropButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smCrop;  writeln('crop button');
  StatusBar.Panels[0].Text := 'CROP';
  SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
end;

procedure TMainForm.DelPageMenuItemClick ( Sender: TObject ) ;
var
  DelPage: LongInt;
  NewPage: Integer;
begin
  if ThumbnailListBox.ItemIndex<0 then exit;
  DelPage := ThumbnailListBox.ItemIndex;
  NewPage := 0;
  if MessageDlg('Confirm delete page',
                'Are you sure that you want to delete page ' + IntToStr(DelPage+1) + ' ?',
                mtConfirmation,
                mbYesNoCancel, 0)=mrYes then
                begin
                  if ThumbnailListBox.Count>1 then
                     begin
                       if DelPage>0 then NewPage := DelPage-1
                       else NewPage := 0;
                     end
                  else NewPage := -1;
                  ThumbnailListBox.Items.Delete(DelPage);
                  ThumbnailListBox.ItemIndex := NewPage;
                  Project.DeletePage(DelPage);
                  if ThumbnailListBox.Count>0
                     then ThumbnailListBoxClick(ThumbnailListBox)
                     else ICanvas.Picture := nil;
                  if ThumbnailListBox.Count>1
                   then pagecountLabel.Caption := Format('%d pages', [ThumbnailListBox.Count])
                   else pagecountLabel.Caption := #32;  // if label is empty it changes its height
                  DelPageButton.Enabled := ThumbnailListBox.Count>0;
                end;
end;

procedure TMainForm.DelSelectButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smDelete;
  StatusBar.Panels[0].Text := 'DELETE';
  SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
end;

procedure TMainForm.ExitMenuItemClick ( Sender: TObject ) ;
begin
  Close;
end;

procedure TMainForm.ExportModeMenuClick ( Sender: TObject ) ;
begin
  case TComponent(Sender).Tag of
       0: ExportButton.OnClick := @DjvuButtonClick;
  end;
  ExportButton.ImageIndex := TMenuItem(Sender).ImageIndex;
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
  DraggingThumbnail := false;
end;

procedure TMainForm.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  {for x := 0 to ThumbnailListBox.Items.Count-1 do
    TBitmap(ThumbnailListBox.Items.Objects[x]).Free;}  //NB: these are now freed by TOcrivistPage;
  if Assigned(Project)
    then Project.Free;
  if Assigned(ScannerHandle) then
      begin
        sane_close(ScannerHandle);
        sane_exit;
      end;
  if PDevices <> nil then Freemem(PDevices);
end;

procedure TMainForm.FormKeyDown ( Sender: TObject; var Key: Word;
  Shift: TShiftState ) ;
begin
  if ICanvas.SelectionMode=smCrop then
     begin
       if Key=27 then begin SelTextButtonClick(SelModeSelectButton); ICanvas.ClearSelection; end
       else if Key=13 then begin DoCrop; SelTextButtonClick(SelModeSelectButton); end;
     end;
end;

procedure TMainForm.LoadModeOptionClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
       0: AddPageButton.OnClick := @LoadPageMenuItemClick;
       1: AddPageButton.OnClick := @ScanPageMenuItemClick;
  end;
  AddPageButton.ImageIndex := TMenuItem(Sender).ImageIndex;
end;

procedure TMainForm.RotateButtonClick ( Sender: TObject ) ;
var
  newpix: PLPix;
  oldpix: PLPix;
  direction: LongInt;
begin
 if Project.CurrentPage=nil then exit;
 direction := 0;
 newpix := nil;
 direction := TComponent(Sender).Tag;
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
  w, h: Longint;
  fn: String;
  HiddenText: PChar;
  x: Integer;
  data: TStringList;
  lin: TLineData;
  lline: Integer;
  wword: Integer;

  function DjvuescapeText( inStr: string ): string;
  var
    cc: Integer;
  begin
    for cc := length(inStr) downto 1 do
       begin
         if inStr[cc]='"'
            then insert('\', inStr, cc)
         else if inStr[cc]='\'
            then insert('\', inStr, cc)
         else if inStr[cc]<#31
            then Delete(inStr, cc, 1);
       end;
    Result := inStr;
  end;
begin
 with SaveDialog do
      begin
        DefaultExt := '.djvu';
        Filter := 'Djvu Files|*.djvu|All Files|*';
        Title := 'Export project to djvu...';
      end;
 if SaveDialog.Execute then
     for x := 0 to Project.PageCount-1 do
        try
          HiddenText := nil;
          data := TStringList.Create;
          data.Add('select; remove-txt');
          data.Add('# -------------------------------------');
          data.Add('select 1');
          data.Add('set-txt');

          p := Project.Pages[x].PageImage;
          pixGetDimensions(p, @w, @h, @d);
          if d=1
             then fn := '/tmp/test.pnm'
             else fn := '/tmp/test.pbm';
          StatusBar.Panels[1].Text := 'Processing page ' + IntToStr(x+1);
          Application.ProcessMessages;
          if Project.Pages[x].OCRData <> nil then
             begin
                data.Add(Format('(page 0 0 %d %d', [w, h]));
                for lline := 0 to Project.Pages[x].OCRData.Linecount-1 do
                   if Project.Pages[x].OCRData.Lines[lline].WordCount>0 then
                   begin
                     lin := Project.Pages[x].OCRData.Lines[lline];
                     data.Add( Format(' (line %d %d %d %d', [lin.Box.Left,
                                                             h-lin.Box.Bottom,
                                                             lin.Box.Right,
                                                             h-lin.Box.Top]));
                     for wword := 0 to lin.WordCount-1 do
                         data.Add( Format('  (word %d %d %d %d "%s")', [lin.Words[wword].Box.Left,
                                                             h-lin.Words[wword].Box.Bottom,
                                                             lin.Words[wword].Box.Right,
                                                             h-lin.Words[wword].Box.Top,
                                                             DjvuescapeText( lin.Words[wword].Text )] ));
                     data.Strings[data.Count-1] := data.Strings[data.Count-1] + ')';
                   end;
                data.Strings[data.Count-1] := data.Strings[data.Count-1] + ')';
                HiddenText := '/tmp/test.txt';
                data.SaveToFile(HiddenText);
             end;
          pixWrite(PChar(fn), p, IFF_PNM);
          if FileExistsUTF8(SaveDialog.FileName)
             then DeleteFileUTF8(SaveDialog.FileName);
          if djvumakepage(fn, '/tmp/test.djvu', HiddenText)=0
             then djvuaddpage(SaveDialog.FileName, '/tmp/test.djvu')
             else ShowMessage('Error when encoding page ' + IntToStr(x+1));
        finally
          StatusBar.Panels[1].Text := '';
          data.Free;
        end;
end;

procedure TMainForm.DeskewButtonClick ( Sender: TObject ) ;
var
  oldpix: PLPix;
  newpix: PLPix;
begin
  if Project.CurrentPage=nil then exit;
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
  SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
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

procedure TMainForm.ThumbnailListBoxDragDrop ( Sender, Source: TObject; X,
  Y: Integer ) ;
var
    DropPosition, StartPosition: Integer;
    DropPoint: TPoint;
 begin
    DropPoint.X := X;
    DropPoint.Y := Y;
    DraggingThumbnail := false;
    with Source as TListBox do
      begin
        StartPosition := ItemAtPos(ThumbnailStartPoint, True) ;
        DropPosition := ItemAtPos(DropPoint, True) ;
        writeln(StartPosition, ' --> ', DropPosition);
        Items.Move(StartPosition, DropPosition) ;
        // Now apply move to contents of TOcrivistProject, too
        Project.MovePage(StartPosition, DropPosition);
        ThumbnailListBox.ItemIndex := DropPosition;
        ThumbnailListBoxClick( ThumbnailListBox );
      end;
 end;

procedure TMainForm.ThumbnailListBoxDragOver ( Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean ) ;
begin
  Accept := Source = ThumbnailListBox;
  TListBox(Sender).ItemIndex := TListBox(Sender).ItemAtPos(Point(X, Y), true);
end;

procedure TMainForm.ThumbnailListBoxMouseDown ( Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
  ThumbnailStartPoint.X := X;
  ThumbnailStartPoint.Y := Y;
  DraggingThumbnail := true;
end;

procedure TMainForm.ThumbnailListBoxMouseUp ( Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
    DraggingThumbnail := false;
end;

procedure TMainForm.UpdateScannerStatus ( Sender: TObject ) ;
begin
  ScanPageMenuItem.Enabled := ScannerHandle<>nil;
  if ScannerHandle=nil then writeln('ScannerHandle is nil')
  else writeln('ScannerHandle ok');
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
  if Project.CurrentPage.OCRData<>nil
     then SynMemo1.Text := Project.CurrentPage.Text;
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
  TListbox(Control).Canvas.TextOut(2, ARect.Top + 2, IntToStr(Index+1) + #32 + TListbox(Control).Items[Index]);
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
  if OpenDialog.Execute then
    begin
      Project.LoadfromFile(OpenDialog.FileName);
      ICanvas.Picture := Project.Pages[0].PageImage;
    end;
end;

procedure TMainForm.SaveAsMenuItemClick ( Sender: TObject ) ;
begin
 with SaveDialog do
      begin
        DefaultExt := '.ovp';
        Filter := 'Ocrivist Projects|*.ovp|All Files|*';
        Title := 'Save project as ..';
      end;
  if SaveDialog.Execute
    then Project.SaveToFile(SaveDialog.FileName);
end;

procedure TMainForm.ScanPageMenuItemClick ( Sender: TObject ) ;
var
  newpage: PLPix;
  nametext: String;
begin
  if ScannerHandle=nil then ScannerForm.ShowModal;

  if ScannerHandle<>nil then
     begin
       writeln('ScannerHandle OK');
       newpage := ScanToPix(ScannerHandle, ScannerForm.GetResolution, ScannerForm.GetColorMode, @ShowScanProgress);
       nametext := Format('scan_%.3d', [ScannerForm.GetNextCounterValue]);
       pixSetText(newpage, PChar( nametext ));
       if newpage<>nil
          then LoadPage(newpage)
       else ShowMessage('Scan page failed');
       StatusBar.Panels[1].Text := '';
     end;
end;

procedure TMainForm.SetScannerMenuItemClick ( Sender: TObject ) ;
begin
 if ScannerHandle=nil then ScannerForm.FormCreate(nil);
 ScannerForm.ShowModal;
end;

procedure TMainForm.TestTesseractButtonClick ( Sender: TObject ) ;
var
  OCRJob: TTesseractPage;
  x: Integer;
begin
  OCRJob := TTesseractPage.Create(Project.CurrentPage.PageImage);
  OCRProgressCount := 0;
  OCRJob.OnOCRLine := @ShowOCRProgress;
  for x := 0 to Project.CurrentPage.SelectionCount-1 do
      OCRJob.RecognizeRect( Project.CurrentPage.Selection[x] );
  Project.CurrentPage.OCRData := OCRJob;
  SynMemo1.Text := Project.CurrentPage.Text;
  StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.ViewMenuItemClick ( Sender: TObject ) ;
begin
  Case TComponent(Sender).Tag of
       0: ICanvas.Mode := vmFitToHeight;   // Fit to Height
       1: ICanvas.Mode := vmFitToWidth;    // Fit to Width
       else
         ICanvas.Scale := TMenuItem(Sender).Tag/100;
       end;
end;

procedure TMainForm.MakeSelection ( Sender: TObject ) ;
var
  S: TSelector;
begin
  if ICanvas.SelectionMode=smSelect then
    begin
      ICanvas.AddSelection(ICanvas.CurrentSelection);
      Project.CurrentPage.AddSelection(ICanvas.CurrentSelection);
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
 thumbBMP := Project.CurrentPage.Thumbnail;
 if thumbBMP<>nil then
   begin
     ThumbnailListBox.Items.Objects[ThumbnailListBox.ItemIndex] := thumbBMP;
     ThumbnailListBox.Invalidate;
   end;
end;

procedure TMainForm.ShowScanProgress ( progress: Single ) ;
var
  percentcomplete: Integer;
begin
  percentcomplete := Trunc(progress*100);
  StatusBar.Panels[1].Text := 'Scanning: ' + IntToStr(percentcomplete) + '%';
  Application.ProcessMessages;
end;

procedure TMainForm.ShowOCRProgress ( progress: Single ) ;
begin
  Inc(OCRProgressCount);
  StatusBar.Panels[1].Text := 'Processing line ' + IntToStr(OCRProgressCount);
  Application.ProcessMessages;
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
 SynMemo1.Clear;
 Project.AddPage(ICanvas.Picture);
 thumbBMP := Project.CurrentPage.Thumbnail;
 if thumbBMP<>nil then
   begin
     ThumbnailListBox.Items.AddObject(newpage^.text, thumbBMP);
   end;
 ThumbnailListBox.ItemIndex := ThumbnailListBox.Count-1;
 if ThumbnailListBox.Count>1
  then pagecountLabel.Caption := Format('%d pages', [ThumbnailListBox.Count])
  else pagecountLabel.Caption := #32;  // if label is empty it changes its height
 AddingThumbnail := false;
 DelPageButton.Enabled := ThumbnailListBox.Count>0;
end;

procedure TMainForm.DoCrop;
var
  newpix: PLPix;
  oldpix: PLPix;
begin
 if Project.CurrentPage=nil then exit;
 newpix := nil;
 oldpix := ICanvas.Picture;
 newpix := CropPix( oldpix,  ICanvas.CurrentSelection );
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

