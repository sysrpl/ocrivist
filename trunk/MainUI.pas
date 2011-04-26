unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, leptonica, LibLeptUtils, pageviewer, types, LCLType, OcrivistData;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem1: TMenuItem;
    ExitMenuItem: TMenuItem;
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
    procedure FormCreate ( Sender: TObject ) ;
    procedure FormDestroy ( Sender: TObject ) ;
    procedure ListBox1DrawItem ( Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState ) ;
  private
    { private declarations }
    CurrentProject: TOcrivistProject;
  public
    { public declarations }
    procedure PaintCanvas(Sender: TObject);
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
begin
  if OpenDialog1.Execute
                        then  begin
                          writeln('loading');
                          ICanvas.Picture := pixRead(PChar(OpenDialog1.FileName));
                          writeln('loaded');
                          thumbBMP := TBitmap.Create;
                          w := 0; //ListBox1.ClientWidth-6;
                          h := THUMBNAIL_HEIGHT;
                          thumbPIX := pixScaleToSize(ICanvas.Picture, w, h);
                          ScaleToBitmap(thumbPIX, thumbBMP, 1);
                          ListBox1.Items.AddObject(IntToStr(ListBox1.Count+1), thumbBMP);
                          ListBox1.ItemIndex := ListBox1.Count-1;
                          pixDestroy(@thumbPIX);
                        end;
end;

procedure TForm1.Button5Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToHeight;
end;

procedure TForm1.Button6Click ( Sender: TObject ) ;
var
  P: TOcrivistProject;
begin
  ICanvas.Mode := vmFitToWidth;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := Panel5;
  ICanvas.Align := alClient;
  ListBox1.Clear;
  ListBox1.ItemIndex := -1;
  ListBox1.ItemHeight := THUMBNAIL_HEIGHT + ListBox1.Canvas.TextHeight('Yy')+2;
  CurrentProject := TOcrivistProject.Create;
  CurrentProject.Title := 'Default Project';
end;

procedure TForm1.FormDestroy ( Sender: TObject ) ;
var
  x: Integer;
begin
  for x := 0 to ListBox1.Items.Count-1 do
    TBitmap(ListBox1.Items.Objects[x]).Free;
  if Assigned(CurrentProject)
    then CurrentProject.Free;
end;

procedure TForm1.ListBox1DrawItem ( Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState ) ;
var
  LeftOffset: Integer;
begin
//  writeln(Index);
  if not assigned(TListbox(Control).Items.Objects[Index]) then exit;
  LeftOffset := (TListbox(Control).ClientWidth-TBitmap(TListbox(Control).Items.Objects[Index]).Width) div 2;
  if LeftOffset<2 then LeftOffset := 2;
  TListbox(Control).Canvas.Rectangle(ARect);
  TListbox(Control).Canvas.Draw(LeftOffset, ARect.Top + TListbox(Control).Canvas.TextHeight('Yy')+2, TBitmap(TListbox(Control).Items.Objects[Index]));
  TListbox(Control).Canvas.TextOut(2, ARect.Top + 2, TListbox(Control).Items[Index]);
  TListbox(Control).Canvas.Pen.Color := clBlack;
  TListbox(Control).Canvas.Frame(ARect);
end;

procedure TForm1.PaintCanvas ( Sender: TObject ) ;
begin
end;

end.

