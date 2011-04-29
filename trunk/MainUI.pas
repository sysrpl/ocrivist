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
    MenuItem1: TMenuItem;
    ExitMenuItem: TMenuItem;
    FitHeightMenuItem: TMenuItem;
    FitWidthMenuItem: TMenuItem;
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
    procedure ViewMenuItemClick ( Sender: TObject ) ;
  private
    { private declarations }
    CurrentProject: TOcrivistProject;
    AddingThumbnail: Boolean;
    procedure MakeSelection ( Sender: TObject );
  public
    { public declarations }
  end;



const
  THUMBNAIL_HEIGHT = 100;

var
  Form1: TForm1;
  ICanvas : TPageViewer;
  Image1:TImage;

  function ScaleRect( aRect: TRect; scale: Single ): TRect;

implementation

function ScaleRect ( aRect: TRect; scale: Single ) : TRect;
var
  TempRect: TRect;
begin
  writeln('scale is ', FormatFloat( '0.00', scale ));
  writeln( Format('aRect: %d %d %d %d', [aRect.Top, aRect.Left, aRect.Bottom, aRect.Right]) );
  TempRect.Top := Round(aRect.Top*scale);
  TempRect.Left := Round(aRect.Left*scale);
  TempRect.Bottom := Round(aRect.Bottom*scale);
  TempRect.Right := Round(aRect.Right*scale);
  Result := TempRect;
end;

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
                          Application.ProcessMessages;  // Draw the screen before doing some background work
                          CurrentProject.AddPage(ICanvas.Picture);
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
  CurrentProject.SaveToFile('/tmp/testproject.ovt');
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
  CurrentProject := TOcrivistProject.Create;
  CurrentProject.Title := 'Default Project';
  AddingThumbnail := false;
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

procedure TForm1.ListBox1Click ( Sender: TObject ) ;
begin
  if AddingThumbnail then exit;
  CurrentProject.ItemIndex := TListBox(Sender).ItemIndex;
  ICanvas.Picture := CurrentProject.CurrentPage.LoadFromTempfile;
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

procedure TForm1.ViewMenuItemClick ( Sender: TObject ) ;
begin
  Case TWinControl(Sender).Tag of
       0: ICanvas.Mode := vmFitToHeight;   // Fit to Height
       1: ICanvas.Mode := vmFitToWidth;    // Fit to Width
       end;
end;

procedure TForm1.MakeSelection ( Sender: TObject ) ;
var
  S: TSelector;
begin
  S := TSelector.Create(ICanvas);
  S.Parent := ICanvas;
  S.Selection := ICanvas.Selection;
//  CurrentProject.CurrentPage.s;
end;


end.

