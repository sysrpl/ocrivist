unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, Buttons, ComCtrls, OcrivistData, selector, Sane, tessintf;

type

  { TForm1 }

  TForm1 = class ( TForm )
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
    StatusBar1: TStatusBar;
    TestTesseractButton: TButton;
    Button6: TButton;
    Button9: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
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
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button6Click ( Sender: TObject ) ;
    procedure Button9Click ( Sender: TObject ) ;
    procedure CropButtonClick ( Sender: TObject ) ;
    procedure DelPageMenuItemClick ( Sender: TObject ) ;
    procedure DelSelectButtonClick ( Sender: TObject ) ;
    procedure ExitMenuItemClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure RotateButtonClick ( Sender: TObject ) ;
    procedure DjvuButtonClick ( Sender: TObject ) ;
    procedure DeskewButtonClick ( Sender: TObject ) ;
    procedure SelTextButtonClick ( Sender: TObject ) ;
    procedure UpdateScannerStatus ( Sender: TObject ) ;
    procedure ListBox1Click ( Sender: TObject ) ;
    procedure ListBox1DrawItem ( Control: TWinControl; Index: Integer;
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
    procedure ShowScanProgress(progress: Single);
    procedure LoadPage( newpage: PLPix );
  public
    { public declarations }
  end;



const
  THUMBNAIL_HEIGHT = 100;

var
  Form1: TForm1;
  ICanvas : TPageViewer;
  Image1:TImage;

 implementation

  uses DjvuUtils, scanner;

  {$R *.lfm}

{ TForm1 }


procedure TForm1.Button6Click ( Sender: TObject ) ;
begin
  //ICanvas.SelectionMode := smSelect;
  writeln('Button6: ', ICanvas.Scale);
  ICanvas.Picture := CropPix(ICanvas.Picture, ScaleRect( ICanvas.Selection , 100/ICanvas.Scale));
  ICanvas.ClearSelection;
end;


procedure TForm1.Button9Click ( Sender: TObject ) ;
var
  BinaryPix: PLPix;
  TextMask: PLPix;
  B: PBoxArray;
  x, y, w, h: Integer;
  count: Integer;
  p: PLPix;
begin
  Textmask := nil;
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
  pixDestroy(@BinaryPix);
end;

procedure TForm1.CropButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smCrop;  writeln('crop button');
end;

procedure TForm1.DelPageMenuItemClick ( Sender: TObject ) ;
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

procedure TForm1.DelSelectButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smDelete;
end;

procedure TForm1.ExitMenuItemClick ( Sender: TObject ) ;
begin
  Close;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := Panel5;
  ICanvas.Align := alClient;
  Icanvas.OnSelect := @MakeSelection;
  ListBox1.Clear;
  ListBox1.ItemIndex := -1;
  ListBox1.ItemHeight := THUMBNAIL_HEIGHT + ListBox1.Canvas.TextHeight('Yy')+2;
  Project := TOcrivistProject.Create;
  Project.Title := 'Default Project';
  AddingThumbnail := false;
end;

procedure TForm1.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  for x := 0 to ListBox1.Items.Count-1 do
    TBitmap(ListBox1.Items.Objects[x]).Free;
  if Assigned(Project)
    then Project.Free;
  if Assigned(ScannerHandle) then
      begin
        sane_close(ScannerHandle);
        sane_exit;
      end;
end;

procedure TForm1.RotateButtonClick ( Sender: TObject ) ;
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
          pixDestroy(@oldpix);
          Project.CurrentPage.PageImage := newpix;
          ICanvas.Picture := newpix;
        end;
   end;
end;

procedure TForm1.DjvuButtonClick ( Sender: TObject ) ;
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
          StatusBar1.Panels[1].Text := 'Processing page ' + IntToStr(x+1);
          Application.ProcessMessages;
          pixWrite(PChar(fn), p, IFF_PNM);
          if djvumakepage(fn, '/tmp/test.djvu')=0
             then djvuaddpage(SaveProjectDialog.FileName, '/tmp/test.djvu')
             else ShowMessage('Error when encoding page ' + IntToStr(x+1));
        finally
          StatusBar1.Panels[1].Text := '';
        end;
end;

procedure TForm1.DeskewButtonClick ( Sender: TObject ) ;
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
       pixDestroy(@oldpix);
     end;
end;

procedure TForm1.SelTextButtonClick ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smSelect;
end;

procedure TForm1.UpdateScannerStatus ( Sender: TObject ) ;
begin
  ScanPageMenuItem.Enabled := ScannerHandle<>nil;
  if ScannerHandle=nil then writeln('ScannerHandle is nil')
  else writeln('ScannerHandle ok');;
end;

procedure TForm1.ListBox1Click ( Sender: TObject ) ;
var
  X: Integer;
  S: TSelector;
begin
  if AddingThumbnail then exit;
  for X := Project.CurrentPage.SelectionCount-1 downto 0 do
    ICanvas.DeleteSelector(X);
  Project.ItemIndex := TListBox(Sender).ItemIndex;
  ICanvas.Picture := Project.CurrentPage.LoadFromTempfile;
  Invalidate;
  for x := 0 to Project.CurrentPage.SelectionCount-1 do
      ICanvas.AddSelection(Project.CurrentPage.Selection[x]);
end;

procedure TForm1.ListBox1DrawItem ( Control: TWinControl; Index: Integer;
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

procedure TForm1.LoadPageMenuItemClick ( Sender: TObject ) ;
var
  newpage: PLPix;
  pagename: String;
  i: Integer;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  OpenDialog1.Filter := 'Image files|*.tif;*.tiff;*.bmp;*.png;*.jpg|All files|*';
  if OpenDialog1.Execute then
     for i := 0 to OpenDialog1.Files.Count-1 do
        begin
          newpage := pixRead(PChar(OpenDialog1.Files[i]));
          if newpage<>nil then
             begin
               pagename :=  ExtractFileNameOnly(OpenDialog1.FileName);
               newpage^.text := PChar(pagename);
               LoadPage(newpage)
             end
           else ShowMessage('Error when loading page ' + OpenDialog1.Files[i]);
        end;
end;

procedure TForm1.OpenProjectMenuItemClick ( Sender: TObject ) ;
begin
  if OpenDialog1.Execute
    then Project.LoadfromFile(OpenDialog1.FileName);
end;

procedure TForm1.SaveAsMenuItemClick ( Sender: TObject ) ;
begin
  if SaveProjectDialog.Execute
    then Project.SaveToFile(SaveProjectDialog.FileName);
end;

procedure TForm1.ScanPageMenuItemClick ( Sender: TObject ) ;
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

procedure TForm1.SetScannerMenuItemClick ( Sender: TObject ) ;
begin
 if ScannerHandle=nil then ScannerForm.FormCreate(nil);
 ScannerForm.ShowModal;
end;

procedure TForm1.TestTesseractButtonClick ( Sender: TObject ) ;
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

procedure TForm1.ViewMenuItemClick ( Sender: TObject ) ;
begin
  Case TMenuItem(Sender).Tag of
       0: ICanvas.Mode := vmFitToHeight;   // Fit to Height
       1: ICanvas.Mode := vmFitToWidth;    // Fit to Width
       else
         ICanvas.Scale := TMenuItem(Sender).Tag;
       end;
end;

procedure TForm1.MakeSelection ( Sender: TObject ) ;
var
  S: TSelector;
begin
  ICanvas.AddSelection(UnScaleRect(ICanvas.Selection, ICanvas.Scale));
  Project.CurrentPage.AddSelection( UnScaleRect (ICanvas.Selection, ICanvas.Scale) )
end;

procedure TForm1.SelectionChange ( Sender: TObject ) ;
var
  SelectionId: LongInt;
begin
  SelectionId := StrToInt( TSelector(Sender).Caption )-1;
  Project.CurrentPage.Selection[SelectionId] := UnScaleRect(TSelector(Sender).Selection, ICanvas.Scale);
end;

procedure TForm1.ShowScanProgress ( progress: Single ) ;
begin
  writeln( FormatFloat('0.00', progress));
end;

procedure TForm1.LoadPage ( newpage: PLPix ) ;
var
  X: Integer;
  thumbBMP: TBitmap;
  w: Integer;
  h: Integer;
  thumbPIX: PLPix;
begin
 ICanvas.Picture := newpage;
 Application.ProcessMessages;  // Draw the screen before doing some background work
 If Project.ItemIndex>=0
  then for X := Project.CurrentPage.SelectionCount-1 downto 0 do
       ICanvas.DeleteSelector(X);
 Project.AddPage(ICanvas.Picture);
 thumbBMP := TBitmap.Create;
 w := 0; //ListBox1.ClientWidth-6;
 h := THUMBNAIL_HEIGHT;
 thumbPIX := pixScaleToSize(ICanvas.Picture, w, h);
 pixWrite('/tmp/tmp.bmp', thumbPIX, IFF_BMP);
 //ScaleToBitmap(thumbPIX, thumbBMP, 1);
 AddingThumbnail := true;  //avoid triggering reload through ThumbList.Click;
 ListBox1.Items.AddObject(newpage^.text, thumbBMP);
 //thumbBMP.SaveToFile('/tmp/tmp/bmp');
 thumbBMP.LoadFromFile('/tmp/tmp.bmp');
 //ListBox1.ItemIndex := ListBox1.Count-1;
 AddingThumbnail := false;
 pixDestroy(@thumbPIX);
end;


end.

