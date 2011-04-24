unit MainUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, leptonica, LibLeptUtils, pageviewer;

type

  { TForm1 }

  TForm1 = class ( TForm )
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit1: TEdit;
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
  private
    { private declarations }
  public
    { public declarations }
    procedure PaintCanvas(Sender: TObject);
  end;

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
begin
  if OpenDialog1.Execute
                        then  begin
                          writeln('loading');
                          ICanvas.Picture := pixRead(PChar(OpenDialog1.FileName));
                          writeln('loaded');
                        end;

end;

procedure TForm1.Button5Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToHeight;
end;

procedure TForm1.Button6Click ( Sender: TObject ) ;
begin
  ICanvas.Mode := vmFitToWidth;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  ICanvas := TPageViewer.Create(self);
  ICanvas.Parent := Panel5;
  ICanvas.Align := alClient;
end;

procedure TForm1.PaintCanvas ( Sender: TObject ) ;
begin
end;

end.

