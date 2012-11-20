unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, Buttons, ComCtrls, SpellCheck,
  OcrivistData, selector, Sane, ocreditor;


type

  { TMainForm }

  TMainForm = class ( TForm )
    AutoselectButton: TToolButton;
    ContrastMenuItem: TMenuItem;
    DeskewButton: TToolButton;
    EnhanceMenuItem: TMenuItem;
    FitHeightButton: TToolButton;
    FitWidthButton: TToolButton;
    LanguageComboBox: TComboBox;
    DeskewMenuItem: TMenuItem;
    CorrectionviewImage: TImage;
    ImageList1: TImageList;
    LoadModeMenu: TPopupMenu;
    LoadModeFileButton: TMenuItem;
    LoadModeScanButton: TMenuItem;
    ExportModeMenu: TPopupMenu;
    ExportDjvuButton: TMenuItem;
    ExportTextButton: TMenuItem;
    ExportPDFButton: TMenuItem;
    ImportMenuItem: TMenuItem;
    CopyTextMenuItem: TMenuItem;
    MenuItem4: TMenuItem;
    ScanSettingsMenuItem: TMenuItem;
    NewProjectMenuItem: TMenuItem;
    OCRScreenMenuItem: TMenuItem;
    ModeMenuItem: TMenuItem;
    ProcessPageMenuItem: TMenuItem;
    LanguagePanel: TPanel;
    Rotate180MenuItem: TMenuItem;
    miAutoProcessPage: TMenuItem;
    miReadPage: TMenuItem;
    miAnalysePage: TMenuItem;
    miAutoAll: TMenuItem;
    OCRMenu: TMenuItem;
    RotateLeftTButton: TToolButton;
    RotateRightButton: TToolButton;
    SaveTextMenuItem: TMenuItem;
    DeleteLineMenuItem: TMenuItem;
    CorrectionPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    CorrectionviewScrollBox: TScrollBox;
    SpellcheckButton: TToolButton;
    Splitter2: TSplitter;
    TesseractButton: TToolButton;
    TextPopupMenu: TPopupMenu;
    pagecountLabel: TLabel;
    MenuItem1: TMenuItem;
    RotateRMenuItem: TMenuItem;
    RotateLMenuItem: TMenuItem;
    SetScannerMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    StatusBar: TStatusBar;
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
    MainToolBar: TToolBar;
    AddPageButton: TToolButton;
    PageToolBar: TToolBar;
    TextToolBar: TToolBar;
    ToolButton1: TToolButton;
    SaveButton: TToolButton;
    DelPageButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ExportButton: TToolButton;
    SelTextButton: TToolButton;
    DelSelectButton: TToolButton;
    CropButton: TToolButton;
    UnsharpMenuItem: TMenuItem;
    View25MenuItem: TMenuItem;
    View100MenuItem: TMenuItem;
    View75MenuItem: TMenuItem;
    View50MenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog: TOpenDialog;
    ListboxPanel: TPanel;
    OCRPanel: TPanel;
    MainPanel: TPanel;
    Splitter1: TSplitter;
    procedure CopyTextMenuItemClick ( Sender: TObject ) ;
    procedure CropButtonClick ( Sender: TObject ) ;
    procedure DelPageMenuItemClick ( Sender: TObject ) ;
    procedure DelSelectButtonClick ( Sender: TObject ) ;
    procedure ExitMenuItemClick ( Sender: TObject ) ;
    procedure ExportModeMenuClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure FormKeyDown ( Sender: TObject; var Key: Word; Shift: TShiftState
      ) ;
    procedure ImportMenuItemClick ( Sender: TObject ) ;
    procedure LanguageComboBoxChange(Sender: TObject);
    procedure LoadModeOptionClick(Sender: TObject);
    procedure miAutoAllClick ( Sender: TObject ) ;
    procedure miAutoProcessPageClick ( Sender: TObject ) ;
    procedure ModeChange ( Sender: TObject ) ;
    procedure NewProjectMenuItemClick ( Sender: TObject ) ;
    procedure RotateButtonClick ( Sender: TObject ) ;
    procedure DjvuButtonClick ( Sender: TObject ) ;
    procedure DeskewButtonClick ( Sender: TObject ) ;
    procedure SaveButtonClick ( Sender: TObject ) ;
    procedure SaveTextMenuItemClick ( Sender: TObject ) ;
    procedure ScanSettingsMenuItemClick ( Sender: TObject ) ;
    procedure SelTextButtonClick ( Sender: TObject ) ;
    procedure AnalyseButtonClick ( Sender: TObject ) ;
    procedure SpellcheckButtonClick ( Sender: TObject ) ;
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
    Editor: TOcrivistEdit;
    Project: TOcrivistProject;
    AddingThumbnail: Boolean;
    ThumbnailStartPoint: TPoint;
    DraggingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
    procedure DeleteSelection ( Sender: TObject );
    procedure SelectionChange ( Sender: TObject );
    procedure UpdateThumbnail ( Sender: TObject );
    procedure ShowScanProgress( progress: Single );
    procedure ShowOCRProgress ( progress: Single );
    procedure LoadPage( newpage: PLPix; pageindex: integer );
    procedure DoCrop;
    function AnalysePage(pageindex: integer): integer;
    procedure OCRPage(pageindex: integer);
    procedure DoSpellcheck;
    procedure EditorSelectToken( aline, aword: integer );
    procedure PopulateLanguageList;
  public
    { public declarations }
  end;



const
  THUMBNAIL_HEIGHT = 100;

var
  MainForm: TMainForm;
  ICanvas : TPageViewer;
  CorrectionviewImage:TImage;
  OCRProgressCount: integer;
  OCRLanguage: string;
  OCRDatapath: string;
  HasDJVU: Boolean;

 implementation

  uses DjvuUtils, scanner, ocr, Clipbrd, progress, scanselect;

  {$R *.lfm}

{ TMainForm }


procedure TMainForm.CropButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smCrop;  writeln('crop button');
  StatusBar.Panels[0].Text := 'CROP';
  //SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
end;

procedure TMainForm.CopyTextMenuItemClick ( Sender: TObject ) ;
begin
  Clipboard.AsText := Editor.Text;
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
  //SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
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
var
  x: Integer;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := MainPanel;
  ICanvas.Align := alClient;
  Icanvas.OnSelect := @MakeSelection;
  Icanvas.OnDeleteSelection := @DeleteSelection;
  ICanvas.OnChangeBitmap := @UpdateThumbnail;
  OCRPanel.Align := alClient;
  ThumbnailListBox.Clear;
  ThumbnailListBox.ItemIndex := -1;
  ThumbnailListBox.ItemHeight := THUMBNAIL_HEIGHT + ThumbnailListBox.Canvas.TextHeight('Yy')+2;
  OCRDatapath := '/usr/local/share/tessdata/';
  PopulateLanguageList;

  OCRLanguage := 'eng';
  LanguageComboBox.ItemIndex :=
             LanguageComboBox.Items.IndexOf(GetLanguageFromToken(OCRLanguage));

  Project := TOcrivistProject.Create;
  Project.Title := 'Untitled';
  AddingThumbnail := false;
  DraggingThumbnail := false;
  Editor := TOcrivistEdit.Create(Self);
  editor.Parent := Panel3;
  editor.Align := alClient;
  Editor.PopupMenu := TextPopupMenu;
  Editor.OnSelectToken := @EditorSelectToken;
  HasDJVU := (SearchFileInPath('djvumake','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[])<>'');
  ExportButton.Enabled := HasDJVU;
  //SelTextButtonClick(SelModeSelectButton);
//  Editor.OnSpellCheck := @SpellCallback;
end;

procedure TMainForm.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  {for x := 0 to ThumbnailListBox.Items.Count-1 do
    TBitmap(ThumbnailListBox.Items.Objects[x]).Free;}  //NB: these are now freed by TOcrivistPage;
  if Assigned(Project)
    then FreeAndNil( Project );
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
       if Key=27 then begin SelTextButtonClick(SelTextButton); ICanvas.ClearSelection; end
      else if Key=13 then begin DoCrop; SelTextButtonClick(SelTextButton); end;
     end;
  if ssCtrl in Shift then if Key=77 then
     begin
       writeln('Key detected: ', Key);
       if OCRScreenMenuItem.Checked
        then ProcessPageMenuItem.Click
        else OCRScreenMenuItem.Click;
       Key := 0;
     end;
end;

procedure TMainForm.ImportMenuItemClick ( Sender: TObject ) ;
var
  pages: LongInt;
  x: Integer;
  newpage: PLPix;
  pagename: String;
  tempfile: String;
begin
  with OpenDialog do
       begin
         DefaultExt := '.djvu';
         Filter := 'DJVU files|*.djvu|All Files|*';
         Title := 'Import pages from DJVU file';
       end;
  if OpenDialog.Execute then
    begin
      pages := djvuGetDocInfo(OpenDialog.FileName).PageCount;
      if pages>0 then
        begin
          Project.Clear;
          ThumbnailListBox.Clear;
          ProgressForm.MainTextLabel.Caption := 'Importing DJVU file: ' + ExtractFileName( OpenDialog.FileName ) +#10
           + '(' + IntToStr(pages) + ' pages)';
          ProgressForm.Show(nil);
          Enabled := false;
        end;
      for x := 1 to pages do
         begin
           newpage := nil;
           ProgressForm.SetUpdateText('Importing page ' + IntToStr(x));
           Application.ProcessMessages;
           tempfile := '/tmp/temppage.tif';
           if FileExistsUTF8(tempfile) then DeleteFileUTF8(tempfile);
           if djvuExtractPage(OpenDialog.FileName, tempfile, x)
              then newpage := pixRead( PChar(tempfile) );
           if newpage <> nil then
             begin
               pagename :=  ExtractFileNameOnly(OpenDialog.FileName)+IntToStr(x);
               pixSetText(newpage, PChar(pagename));
               LoadPage(newpage, ThumbnailListBox.ItemIndex+1);
           //    if x>2 then Project.Pages[x-2].Active := false;
             end;
           if FileExistsUTF8(tempfile) then DeleteFileUTF8(tempfile);
         end;
      ProgressForm.Hide;
      Enabled := true;
    end;
    StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.LanguageComboBoxChange(Sender: TObject);
begin
  OCRLanguage := GetLanguageToken(LanguageComboBox.Items[LanguageComboBox.ItemIndex]);
end;

procedure TMainForm.LoadModeOptionClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
       0: AddPageButton.OnClick := @LoadPageMenuItemClick;
       1: AddPageButton.OnClick := @ScanPageMenuItemClick;
  end;
  AddPageButton.ImageIndex := TMenuItem(Sender).ImageIndex;
  AddPageButton.Hint := TMenuItem(Sender).Hint;
end;

procedure TMainForm.miAutoAllClick ( Sender: TObject ) ;
var
  x: Integer;
begin
  if Project.PageCount<1 then Exit;
  for x := 0 to Project.PageCount-1 do
     begin
       AnalysePage(x);
       OCRPage(x);
     end;
  Editor.OCRData := Project.CurrentPage.OCRData;
end;

procedure TMainForm.miAutoProcessPageClick ( Sender: TObject ) ;
begin
  if AnalysePage(ThumbnailListBox.ItemIndex)>0 then
    begin
      ThumbnailListBoxClick(ThumbnailListBox);
      OCRPage(ThumbnailListBox.ItemIndex);
      Editor.OCRData := Project.CurrentPage.OCRData;
    end;
end;

procedure TMainForm.ModeChange ( Sender: TObject ) ;
begin
  TMenuItem(Sender).Checked := true;
  if Sender=ProcessPageMenuItem then
     begin
       MainPanel.Visible := true;
       OCRPanel.Visible := false;
     end;
  Application.ProcessMessages;
  OCRPanel.Visible := OCRScreenMenuItem.Checked;
  MainPanel.Visible := ProcessPageMenuItem.Checked;
  if MainPanel.Visible then writeln('MainPanel.Visible')
  else WriteLn('MainPanel not visible');
end;

procedure TMainForm.NewProjectMenuItemClick ( Sender: TObject ) ;
begin
  Project.Clear;
  ThumbnailListBox.Clear;
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
 oldpix := Project.CurrentPage.PageImage;

 if (Sender=RotateLeftTButton) or (Sender=RotateLMenuItem)
    then direction := -1
 else if (Sender=RotateRightButton) or (Sender=RotateRMenuItem)
    then direction := 1;
 if direction<>0
    then newpix := pixRotate90(oldpix, direction)
 else newpix := pixRotate180(nil, oldpix);   //Sender=Rotate180MenuItem;
 if newpix<>nil then
    begin
      if oldpix <> nil then pixDestroy(@oldpix);
      Project.CurrentPage.PageImage := newpix;
      ICanvas.Picture := newpix;
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
        FileName := Project.Title;
      end;
 if SaveDialog.Execute then
     begin
       if FileExistsUTF8(SaveDialog.FileName)
             then DeleteFileUTF8(SaveDialog.FileName);
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
               then fn := '/tmp/ocr-temp.pnm'
               else fn := '/tmp/ocr-temp.pbm';
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
                  HiddenText := '/tmp/ocr-temp.txt';
                  data.SaveToFile(HiddenText);
               end;
            pixWrite(PChar(fn), p, IFF_PNM);
            if djvumakepage(fn, '/tmp/ocr-temp.djvu', HiddenText)=0
               then djvuaddpage(SaveDialog.FileName, '/tmp/ocr-temp.djvu')
               else ShowMessage('Error when encoding page ' + IntToStr(x+1));
          finally
            StatusBar.Panels[1].Text := '';
            data.Free;
            DeleteFileUTF8(fn);
            DeleteFileUTF8(HiddenText);
            DeleteFileUTF8('/tmp/ocr-temp.djvu');
          end;
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

procedure TMainForm.SaveButtonClick ( Sender: TObject ) ;
var
  x: Integer;
   I: Integer;
begin
  if Project.Filename=''
     then SaveAsMenuItemClick(Sender)
  else
    begin
      Project.SaveToFile(Project.Filename);
    end;
end;

procedure TMainForm.SaveTextMenuItemClick ( Sender: TObject ) ;
begin
 with SaveDialog do
      begin
        DefaultExt := '.txt';
        Filter := 'All Files|*';
        Title := 'Save page text as ...';
      end;
  if SaveDialog.Execute then
     begin
       Editor.Lines.SaveToFile(SaveDialog.FileName);
     end;
end;

procedure TMainForm.ScanSettingsMenuItemClick ( Sender: TObject ) ;
begin
  ScannerForm.ShowModal;
end;

procedure TMainForm.SelTextButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smSelect;
  StatusBar.Panels[0].Text := 'SELECT';
  SelTextButton.Down := true;
  //SelectToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
end;

procedure TMainForm.AnalyseButtonClick ( Sender: TObject ) ;
begin
 if AnalysePage(ThumbnailListBox.ItemIndex)>0
  then ThumbnailListBoxClick(ThumbnailListBox);
end;

procedure TMainForm.SpellcheckButtonClick ( Sender: TObject ) ;
begin
  DoSpellcheck;
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
end;

procedure TMainForm.ThumbnailListBoxClick ( Sender: TObject ) ;
var
  X: Integer;
  S: TSelector;
begin
  if AddingThumbnail then exit;
  if ThumbnailListBox.ItemIndex<0 then Exit;
  for X := Project.CurrentPage.SelectionCount-1 downto 0 do
    ICanvas.DeleteSelector(X);
  Project.ItemIndex := TListBox(Sender).ItemIndex;
  ICanvas.Picture := Project.CurrentPage.PageImage;
  Invalidate;
  for x := 0 to Project.CurrentPage.SelectionCount-1 do
    begin
      ICanvas.AddSelection(Project.CurrentPage.Selection[x]);
      ICanvas.GetSelector(ICanvas.SelectionCount-1).OnSelect := @SelectionChange;
    end;
  editor.OCRData := Project.CurrentPage.OCRData;
  if Project.CurrentPage.OCRData=nil
     then ProcessPageMenuItem.Click;
  CorrectionviewImage.Picture.Clear;
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
  x, i: Integer;
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
               LoadPage(newpage, ThumbnailListBox.ItemIndex+1);
               if not MainPanel.Visible
                  then ProcessPageMenuItem.Click;
               for x := 0 to MainToolBar.ControlCount-1 do
                  if TControl(MainToolBar.Controls[x]).Tag=1
                     then TControl(MainToolBar.Controls[x]).Visible := true;
             end
           else ShowMessage('Error when loading page ' + OpenDialog.Files[i]);
        end;
end;

procedure TMainForm.OpenProjectMenuItemClick ( Sender: TObject ) ;
var
  x: Integer;
begin
  with OpenDialog do
       begin
         DefaultExt := '.ovp';
         Filter := 'Ocrivist Projects|*.ovp|All Files|*';
         Title := 'Open Ocrivist project';
       end;
  if OpenDialog.Execute then
    if Project.LoadfromFile(OpenDialog.FileName)=0 then
      begin
        ICanvas.Scale :=  Project.ViewerScale;
        ThumbnailListBox.Clear;
        for x := 0 to Project.PageCount-1 do
           ThumbnailListBox.Items.AddObject( Project.Pages[x].Title, Project.Pages[x].Thumbnail );
        ThumbnailListBox.ItemIndex := Project.ItemIndex;
        ThumbnailListBoxClick(ThumbnailListBox);
        Caption := 'Ocrivist : ' + Project.Title;
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
  if SaveDialog.Execute then
    begin
      ProgressForm.SetMainText( 'Saving project...' );
      ProgressForm.Show(nil);
      Application.ProcessMessages;
      try
      Project.SaveToFile(SaveDialog.FileName);
      Caption := 'Ocrivist : ' + Project.Title;
      ThumbnailListBox.ItemIndex := Project.ItemIndex;
      finally
        ProgressForm.Hide;
      end;
    end;
end;

procedure TMainForm.ScanPageMenuItemClick ( Sender: TObject ) ;
var
  newpage: PLPix;
  nametext: String;
  resolution: Integer;
begin
  if ScannerHandle=nil then
    begin
      if ScannerSelector.CheckDevices>0
       then ScannerForm.ShowModal;
    end;
  if ScannerHandle<>nil then
    if ScannerForm.SetScannerOptions=0 then
     begin
       writeln('ScannerHandle OK');
       resolution := ScannerForm.GetResolution;
       newpage := ScanToPix(ScannerHandle, @ShowScanProgress);
       pixSetResolution(newpage, resolution, resolution);
       nametext := Format('scan_%.3d', [ScannerForm.GetNextCounterValue]);
       pixSetText(newpage, PChar( nametext ));
       if newpage<>nil
          then LoadPage(newpage, ThumbnailListBox.ItemIndex+1)
       else ShowMessage('Scan page failed');
       StatusBar.Panels[1].Text := '';
     end;
end;

procedure TMainForm.SetScannerMenuItemClick ( Sender: TObject ) ;
begin
 try
   Enabled := false;
   ScanSettingsMenuItem.Enabled := ScannerSelector.CheckDevices>0;
 finally
   Enabled := true;
 end;
 if ScannerSelector.ShowModal=mrOK
    then ScannerForm.ShowModal;
end;

procedure TMainForm.TestTesseractButtonClick ( Sender: TObject ) ;
begin
  if Project.PageCount<1 then Exit;
  Enabled := false;
  ProgressForm.SetMainText('Reading page...');
  ProgressForm.SetUpdateText(' ');
  ProgressForm.Show(nil);
  try
    OCRPage(ThumbnailListBox.ItemIndex);
  finally
    Enabled := true;
    ProgressForm.Hide;
  end;
  Editor.OCRData := Project.CurrentPage.OCRData;
  if not OCRPanel.Visible
     then OCRScreenMenuItem.Click;
end;

procedure TMainForm.ViewMenuItemClick ( Sender: TObject ) ;
begin
  Case TComponent(Sender).Tag of
       0: ICanvas.Mode := vmFitToHeight;   // Fit to Height
       1: ICanvas.Mode := vmFitToWidth;    // Fit to Width
       else
         ICanvas.Scale := TMenuItem(Sender).Tag/100;
       end;
  Project.ViewerScale := ICanvas.Scale;
end;

procedure TMainForm.MakeSelection ( Sender: TObject ) ;
var
  S: TSelector;
begin
  if ICanvas.SelectionMode=smSelect then
    begin
      ICanvas.AddSelection(ICanvas.CurrentSelection);
      ICanvas.GetSelector(ICanvas.SelectionCount-1).OnSelect := @SelectionChange;
      Project.CurrentPage.AddSelection(ICanvas.CurrentSelection);
    end;
end;

procedure TMainForm.DeleteSelection(Sender: TObject);
begin
  writeln('delete selection');
  if Project<>nil then
     Project.CurrentPage.DeleteSelection(ICanvas.SelectionIndex(TSelector(Sender)));
end;

procedure TMainForm.SelectionChange ( Sender: TObject ) ;
var
  SelectionId: LongInt;
begin
  if ICanvas.SelectionMode=smSelect then
    begin
      SelectionId := ICanvas.SelectionIndex(TSelector(Sender));
      ICanvas.SetSelection(SelectionId, TSelector(Sender).Selection);
      Project.CurrentPage.Selection[SelectionId] := UnScaleRect(TSelector(Sender).Selection, ICanvas.Scale);

    end;
end;

// called by ICanvas when page image is changed
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
  //StatusBar.Panels[1].Text := 'Processing line ' + IntToStr(OCRProgressCount);
  ProgressForm.SetUpdateText('Processing line ' + IntToStr(OCRProgressCount));
  Application.ProcessMessages;
end;

procedure TMainForm.LoadPage ( newpage: PLPix; pageindex: integer ) ;
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
 Editor.OCRData := nil;
 Project.AddPage(ICanvas.Picture, pageindex);
 thumbBMP := Project.CurrentPage.Thumbnail;
 if thumbBMP<>nil then
   begin
     ThumbnailListBox.Items.InsertObject(pageindex, newpage^.text, thumbBMP);
   end;
 ThumbnailListBox.ItemIndex := pageindex;
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

function TMainForm.AnalysePage ( pageindex: integer ) : integer;
var
  Pix: PLPix;
  BinaryPix: PLPix;
  TextMask: PLPix;
  B: PBoxArray;
  x, y, w, h: Integer;
  count: Integer;
  p: PLPix;
begin
  Result := 0;
  if (pageindex<0) or (pageindex>Project.PageCount-1) then Exit;
  Textmask := nil;
  BinaryPix := nil;
  Pix := Project.Pages[pageindex].PageImage;
  if pixGetDepth(Pix) > 1
     then BinaryPix := pixThresholdToBinary(Pix, 60)
     else BinaryPix := pixClone(Pix);
  if pixGetRegionsBinary(BinaryPix, nil, nil, @TextMask, 0) = 0 then
     begin
       B := pixConnComp(TextMask, nil, 8);
       try
       boxaWrite('/tmp/boxes.txt', B);
       for count := 0 to boxaGetCount(B)-1 do
            begin
              boxaGetBoxGeometry(B, count, @x, @y, @w, @h);
              Project.Pages[pageindex].AddSelection( Rect(x-10, y-5, x+w+10, y+h+5) );
//              ICanvas.AddSelection(Rect(x-10, y-5, x+w+10, y+h+5));
            end;
       Result := boxaGetCount(B);
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

procedure TMainForm.OCRPage ( pageindex: integer ) ;
var
  OCRJob: TTesseractPage;
  x: Integer;
begin
  if (pageindex<0) or (pageindex>Project.PageCount-1) then Exit;
  try
    writeln(OCRLanguage);
    OCRJob := TTesseractPage.Create(Project.Pages[pageindex].PageImage, PChar(OCRDatapath), PChar(OCRLanguage));
    OCRProgressCount := 0;
    OCRJob.OnOCRLine := @ShowOCRProgress;
    for x := 0 to Project.Pages[pageindex].SelectionCount-1 do
        OCRJob.RecognizeRect( Project.Pages[pageindex].Selection[x] );
    Project.Pages[pageindex].OCRData := OCRJob;
   // Editor.OCRData := Project.CurrentPage.OCRData;
    //MainPanel.Visible := false;
    OCRPanel.Visible := true;
    StatusBar.Panels[1].Text := '';
  except
    writeln('Error in OCRPage');
  end;
end;

procedure TMainForm.DoSpellcheck;
begin
  Editor.Spellcheck;
end;

procedure TMainForm.EditorSelectToken ( aline, aword: integer ) ;
var
  topline: Integer;
  selTop: LongInt;
  selBottom: Integer;
  selBox: PLBox;
  tmpPix: PLPix;
  pixd: PLPix;
  wtop: Integer;
  wleft: Integer;
  wwidth: Integer;
  wheight: Integer;
  pixwidth : Integer;
begin
  if aline>0 then
    begin
      topline := aline-1;
      selTop := Editor.OCRData.Lines[topline].Box.Top;
    end
  else
    begin
      topline := aline;
      selTop := Editor.OCRData.Lines[aline].Box.Top;
    end;
  if aline<Editor.OCRData.Linecount-1
   then selBottom := Editor.OCRData.Lines[topline+2].Box.Bottom-selTop
   else selBottom := Editor.OCRData.Lines[aline].Box.Bottom-selTop;
  selBox := boxCreate(Editor.OCRData.Lines[aline].Box.Left,
            selTop,
            Editor.OCRData.Lines[aline].Box.Right - Editor.OCRData.Lines[aline].Box.Left,
            selBottom);
  tmpPix := pixClipRectangle(Project.CurrentPage.PageImage, selBox, nil);
  pixd := pixConvertTo32(tmpPix);
  boxDestroy(@selBox);
  wtop :=  Editor.OCRData.Lines[aline].Words[aword].Box.Top - selTop;
  wleft := Editor.OCRData.Lines[aline].Words[aword].Box.Left - Editor.OCRData.Lines[aline].Box.Left;
  wwidth := Editor.OCRData.Lines[aline].Words[aword].Box.Right - Editor.OCRData.Lines[aline].Words[aword].Box.Left;
  wheight := Editor.OCRData.Lines[aline].Words[aword].Box.Bottom - Editor.OCRData.Lines[aline].Words[aword].Box.Top;
  selBox := boxCreate(wleft-5, wtop-5, wwidth+10, wheight+10);
  pixRenderBoxBlend(pixd, selBox, 4, 255, 150, 0, 0.8);
  pixGetDimensions(pixd, @pixwidth, nil, nil);
  ScaleToBitmap(pixd, CorrectionviewImage.Picture.Bitmap, CorrectionPanel.Width/pixwidth) ;
  boxDestroy(@selBox);
  pixDestroy(@tmpPix);
  pixDestroy(@pixd);
end;

procedure TMainForm.PopulateLanguageList;
Var
   SearchResult : TSearchRec;
   Filemask: String;
   token: String;
begin
  LanguageComboBox.Items.Clear;
  Filemask := OCRDatapath + '*.traineddata';
  writeln (Filemask);
  If FindFirst (Filemask, (faAnyFile And Not faDirectory) , SearchResult) = 0 Then
     try
       token := ExtractFileNameWithoutExt(SearchResult.Name);
       LanguageComboBox.Items.Add(GetLanguageFromToken(token));
       While FindNext (SearchResult) = 0 Do
             Begin
             token := ExtractFileNameWithoutExt(SearchResult.Name);
             LanguageComboBox.Items.Add(GetLanguageFromToken(token));
             End;
     finally
       FindClose (SearchResult);
     end;
end;



end.

