unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType,
  ActnList, OcrivistData, selector;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
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
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure ListBox1Click ( Sender: TObject ) ;
    procedure ListBox1DrawItem ( Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState ) ;
    procedure SaveAsMenuItemClick ( Sender: TObject ) ;
    procedure ViewMenuItemClick ( Sender: TObject ) ;
  private
    { private declarations }
    Project: TOcrivistProject;
    AddingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
    procedure SelectionChange ( Sender: TObject );
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

procedure TForm1.SaveAsMenuItemClick ( Sender: TObject ) ;
begin
  if SaveProjectDialog.Execute
    then Project.SaveToFile(SaveProjectDialog.FileName);
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


end.

