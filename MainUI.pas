unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, OcrivistData, selector, Sane, tessintf;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    SetScannerMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    TestDJVUButton: TButton;
    TestTesseractButton: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
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
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click ( Sender: TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure Button3Click ( Sender: TObject ) ;
    procedure Button4Click ( Sender: TObject ) ;
    procedure Button5Click ( Sender: TObject ) ;
    procedure Button6Click ( Sender: TObject ) ;
    procedure Button7Click ( Sender: TObject ) ;
    procedure Button8Click ( Sender: TObject ) ;
    procedure Button9Click ( Sender: TObject ) ;
    procedure DelPageMenuItemClick ( Sender: TObject ) ;
    procedure ExitMenuItemClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure UpdateScannerStatus ( Sender: TObject ) ;
    procedure ListBox1Click ( Sender: TObject ) ;
    procedure ListBox1DrawItem ( Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState ) ;
    procedure LoadPageMenuItemClick ( Sender: TObject ) ;
    procedure OpenProjectMenuItemClick ( Sender: TObject ) ;
    procedure SaveAsMenuItemClick ( Sender: TObject ) ;
    procedure ScanPageMenuItemClick ( Sender: TObject ) ;
    procedure SetScannerMenuItemClick ( Sender: TObject ) ;
    procedure TestDJVUButtonClick ( Sender: TObject ) ;
    procedure TestTesseractButtonClick ( Sender: TObject ) ;
    procedure ViewMenuItemClick ( Sender: TObject ) ;
  private
    { private declarations }
    Project: TOcrivistProject;
    AddingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
    procedure SelectionChange ( Sender: TObject );
    procedure ShowScanProgress(progress: Single);
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

procedure TForm1.Button1Click ( Sender: TObject ) ;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := Self;
  ICanvas.Top := 100;
end;

procedure TForm1.Button2Click ( Sender: TObject ) ;
begin
  if OpenDialog1.Execute
                        then Image1.Picture.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.Button3Click ( Sender: TObject ) ;
begin
  ICanvas.Height := ICanvas.Height+10;
end;

procedure TForm1.Button4Click ( Sender: TObject ) ;
var
  thumbBMP: TBitmap;
  w: Integer;
  thumbPIX: Pointer;
  h: Integer;
  X: Integer;
begin
  if OpenDialog1.Execute
                        then  begin
                          writeln('loading');
                          ICanvas.Picture := pixRead(PChar(OpenDialog1.FileName));
                          writeln('loaded');
                          Application.ProcessMessages;  // Draw the screen before doing some background work
                          If Project.ItemIndex>=0
                           then for X := Project.CurrentPage.SelectionCount-1 downto 0 do
                                ICanvas.DeleteSelector(X);
                          Project.AddPage(ICanvas.Picture);
                          thumbBMP := TBitmap.Create;
                          w := 0; //ListBox1.ClientWidth-6;
                          h := THUMBNAIL_HEIGHT;
                          thumbPIX := pixScaleToSize(ICanvas.Picture, w, h);
                          ScaleToBitmap(thumbPIX, thumbBMP, 1);
                          AddingThumbnail := true;  //avoid triggering reload through ThumbList.Click;
                          ListBox1.Items.AddObject(IntToStr(ListBox1.Count+1), thumbBMP);
                          ListBox1.ItemIndex := ListBox1.Count-1;
                          AddingThumbnail := false;
                          pixDestroy(@thumbPIX);
                        end;
end;

procedure TForm1.Button5Click ( Sender: TObject ) ;
begin
  ICanvas.SelectionMode := smCrop;
//  InitWithLanguage(PChar('eng'));
end;

procedure TForm1.Button6Click ( Sender: TObject ) ;
begin
  //ICanvas.SelectionMode := smSelect;
  writeln('Button6: ', ICanvas.Scale);
  ICanvas.Picture := CropPix(ICanvas.Picture, ScaleRect( ICanvas.Selection , 100/ICanvas.Scale));
  ICanvas.ClearSelection;
end;

procedure TForm1.Button7Click ( Sender: TObject ) ;
begin
  Project.SaveToFile('/tmp/testproject.ovt');
end;

procedure TForm1.Button8Click ( Sender: TObject ) ;
begin
  ICanvas.Picture := pixDeskew(Icanvas.Picture, 0);
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
  thumbBMP: TBitmap;
  w: Integer;
  thumbPIX: Pointer;
  h: Integer;
  X: Integer;
begin
  if OpenDialog1.Execute
                        then  begin
                          writeln('loading');
                          ICanvas.Picture := pixRead(PChar(OpenDialog1.FileName));
                          writeln('loaded');
                          Application.ProcessMessages;  // Draw the screen before doing some background work
                          If Project.ItemIndex>=0
                           then for X := Project.CurrentPage.SelectionCount-1 downto 0 do
                                ICanvas.DeleteSelector(X);
                          Project.AddPage(ICanvas.Picture);
                          thumbBMP := TBitmap.Create;
                          w := 0; //ListBox1.ClientWidth-6;
                          h := THUMBNAIL_HEIGHT;
                          thumbPIX := pixScaleToSize(ICanvas.Picture, w, h);
                          ScaleToBitmap(thumbPIX, thumbBMP, 1);
                          AddingThumbnail := true;  //avoid triggering reload through ThumbList.Click;
                          ListBox1.Items.AddObject(IntToStr(ListBox1.Count+1), thumbBMP);
                          ListBox1.ItemIndex := ListBox1.Count-1;
                          AddingThumbnail := false;
                          pixDestroy(@thumbPIX);
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
  X: Integer;
  thumbBMP: TBitmap;
  w: Integer;
  h: Integer;
  thumbPIX: Pointer;
begin
  if ScannerHandle<>nil then
     begin
       writeln('ScannerHandle OK');
       ICanvas.Picture := ScanToPix(ScannerHandle, ScannerForm.GetResolution, ScannerForm.GetColorMode, @ShowScanProgress);
       If Project.ItemIndex>=0
        then for X := Project.CurrentPage.SelectionCount-1 downto 0 do
             ICanvas.DeleteSelector(X);
       Project.AddPage(ICanvas.Picture);
       thumbBMP := TBitmap.Create;
       w := 0; //ListBox1.ClientWidth-6;
       h := THUMBNAIL_HEIGHT;
       thumbPIX := pixScaleToSize(ICanvas.Picture, w, h);
       ScaleToBitmap(thumbPIX, thumbBMP, 1);
       AddingThumbnail := true;  //avoid triggering reload through ThumbList.Click;
       ListBox1.Items.AddObject(IntToStr(ListBox1.Count+1), thumbBMP);
       ListBox1.ItemIndex := ListBox1.Count-1;
       AddingThumbnail := false;
       pixDestroy(@thumbPIX);
     end;
end;

procedure TForm1.SetScannerMenuItemClick ( Sender: TObject ) ;
begin
  ScannerForm.ShowModal;
end;

procedure TForm1.TestDJVUButtonClick ( Sender: TObject ) ;
var
  p: Pointer;
  d: LongInt;
  fn: String;
begin
  p := ICanvas.Picture;
  d := pixGetDepth(p);
  if d=1
     then fn := '/tmp/test/pnm'
     else fn := '/tmp/test.pbm';
  pixWrite(PChar(fn), p, IFF_PNM);
  writeln( djvumakepage(fn, '/tmp/test.djvu') );
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
  Case TWinControl(Sender).Tag of
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


end.

