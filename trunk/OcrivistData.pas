unit OcrivistData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, leptonica, selector;

type

  { TOcrivistPage }

  TOcrivistPage = class(TObject)
  private
    FTitle: string;
    FTempFile: TFilename;
    FText: TStringlist;
    FSelections: array of TRect;
    FModified: Boolean;
  private
    function GetSelection ( aIndex: Integer ) : TRect;
    function GetSelectionCount: Integer;
    procedure SetPageImage ( const AValue: PLPix ) ;
    procedure SetSelection ( aIndex: Integer ; const AValue: TRect ) ;
  public
    constructor Create( pix: PLPix );
    destructor Destroy; override;
    function LoadFromTempfile: PLPix;
    procedure SaveToTempfile ( Pix: PLPix; Path: String );
    procedure AddSelection ( Sel: TRect );
    procedure DeleteSelection ( aIndex: Integer );
    property Text: TStringList read FText write FText;
    property Selection[ aIndex: Integer ]: TRect read GetSelection write SetSelection;
    property SelectionCount: Integer read GetSelectionCount;
    property Title: string read FTitle write FTitle;
    property Modified: Boolean read FModified write FModified;
    property PageImage: PLPix read LoadFromTempfile write SetPageImage;
  end;

  { TOcrivistProject }

  TOcrivistProject = class( TObject )
  private
    FTitle: string;
    FPages: array of TOcrivistPage;
    FFilename: string;
    FTempFolder: string;
    FcurrentPage: Integer;
    FPageWidth: Integer;
    FPageHeight: Integer;
    function GetCurrentPage: TOcrivistPage;
    function GetPage ( aIndex: Integer ) : TOcrivistPage;
    function GetPageCount: Integer;
    procedure PutPage ( aIndex: Integer; const AValue: TOcrivistPage ) ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile( FileName: TFilename );
    function LoadfromFile( FileName: TFilename ): integer;
    procedure DeletePage( aIndex: Integer );
    procedure AddPage( Pix: PLPix );
    property Pages[aIndex: Integer]: TOcrivistPage read GetPage write PutPage;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FPageWidth write FPageWidth;
    property Height: Integer read FPageHeight write FPageHeight;
    property CurrentPage: TOcrivistPage read GetCurrentPage;
    property ItemIndex: integer read FcurrentPage write FcurrentPage;
    property PageCount: Integer read GetPageCount;
  end;

implementation

{ TOcrivistProject }

function TOcrivistProject.GetPage ( aIndex: Integer ) : TOcrivistPage;
begin
  Result := nil;
  if aIndex >=  0
     then if aIndex < Length(FPages)
        then Result := FPages[aIndex];
end;

function TOcrivistProject.GetCurrentPage: TOcrivistPage;
begin
  Result := FPages[FcurrentPage];
end;

function TOcrivistProject.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

procedure TOcrivistProject.PutPage ( aIndex: Integer;
  const AValue: TOcrivistPage ) ;
begin
  if (aIndex>=0) and (aIndex<PageCount)
     then FPages[aIndex] := AValue
     else Raise Exception.Create('Page index out of range (' + IntToStr(aIndex) + ')');
end;

constructor TOcrivistProject.Create;
begin
  FcurrentPage := -1;
  FPageWidth := 0;
  FPageHeight := 0;
end;

destructor TOcrivistProject.Destroy;
var
  c: Integer;
begin
  for c := Length(FPages)-1 downto 0 do
    TOcrivistPage(FPages[c]).Free;
  inherited Destroy;
end;

procedure TOcrivistProject.SaveToFile ( FileName: TFilename ) ;
var
  F: THandle;
  databuf: string;
  tempbuf: PByteArray;
  bytes: Integer;
  page: Integer;
  aPage: TOcrivistPage;
  bytecount: Integer;
  pageImage: PLPix;
begin
  if FileExists(FileName)
     then DeleteFile(FileName);
  F := FileCreate(FileName);
  if F > 0 then
     try
       databuf := 'OVT   ';
       FileWrite(F, databuf[1], Length(databuf));              //1 - 6 bytes - file identifier - spare bytes are for version info
       FileWrite(F, FcurrentPage, SizeOf(FcurrentPage));       //2 - integer - current page
       FileWrite(F, FPageWidth, SizeOf(FPageWidth));           //3 - Integer - page width
       FileWrite(F, FPageHeight, SizeOf(FPageHeight));         //4 - Integer - page height;
       bytes := Length(FTitle);
       FileWrite(F, bytes, SizeOf(bytes));                     //5 - Integer - length of string FTitle
       FileWrite(F, FTitle[1], Length(FTitle));                //6 - array of char - string FTitle
       bytes := Length(FPages);
       FileWrite(F, bytes, SizeOf(bytes));                     //7 - Integer - number of pages in project
       for page := 0 to Length(FPages)-1 do
         begin
           aPage := TOcrivistPage(FPages[page]);
           databuf := aPage.FTitle;
           bytes := Length(databuf);
           FileWrite(F, bytes, SizeOf(bytes));                 //1 - Integer - length of string Title
           FileWrite(F, databuf[1], bytes);                       //2 - array of char - string Title
           bytes := Length(aPage.Text.Text);
           FileWrite(F, bytes, SizeOf(bytes));                 //3 - Integer - length of string Text
           databuf := aPage.Text.Text;                         //4 - array of char - = string Text
           FileWrite(F, databuf[1], bytes);
           pageImage := pixRead(PChar( aPage.FTempFile ));
           if pageImage<>nil then
              try
                if pixWriteMem(@tempbuf, @bytecount, pageImage, IFF_TIFF_ZIP)=0 then
                    begin
                      writeln('writing image data ');
                      FileWrite(F, bytecount, SizeOf(bytecount));    //5 - Integer - length of image data
                      writeln('written data size: ', bytecount);
                      FileWrite(F, tempbuf^[0], bytecount);          //6 - array of byte - image data
                    end;
              finally
                pixDestroy(@pageImage);
              end
           else
               Raise Exception.Create('unable to open temporary page image ' + aPage.FTempFile);
         end;
     finally
       FileClose(F);
     end
  else Raise Exception.Create('Unable to save project ' + FileName);
end;

function TOcrivistProject.LoadfromFile ( FileName: TFilename ) : integer;
begin
  Result := -1;
end;

procedure TOcrivistProject.DeletePage ( aIndex: Integer ) ;
var
  x: LongInt;
begin
  if (aIndex>=0) and (aIndex<PageCount) then
     begin
       TOcrivistPage(FPages[aIndex]).Free;
       for x := aIndex to PageCount-2 do
         FPages[x] := FPages[x+1];
       SetLength(FPages, PageCount-1);
       if FCurrentPage>=aIndex
          then Dec(FCurrentPage);
       if (FcurrentPage<0) and (PageCount>0) then FcurrentPage := 0;
     end
     else Raise Exception.Create('Page index out of range (' + IntToStr(aIndex) + ')');
end;

procedure TOcrivistProject.AddPage ( Pix: PLPix ) ;
var
  P: TOcrivistPage;
begin
  P := TOcrivistPage.Create(Pix);
  if P <> nil then
     begin
       SetLength(FPages, Length(FPages)+1);
       FPages[PageCount-1] := P;
       FcurrentPage := PageCount-1;
     end;
end;

{ TOcrivistPage }

function TOcrivistPage.GetSelection ( aIndex: Integer ) : TRect;
begin
  if (aIndex >= 0) and (aIndex < Length(FSelections))
     then Result := FSelections[aIndex]
     else Raise Exception.CreateFmt('Error in GetSelection: Index out of range (%d)', [aIndex]);
end;

function TOcrivistPage.GetSelectionCount: Integer;
begin
  Result := Length(FSelections);
end;

procedure TOcrivistPage.SetPageImage ( const AValue: PLPix ) ;
begin
  SaveToTempfile(AValue, '');
end;

procedure TOcrivistPage.SetSelection ( aIndex: Integer ; const AValue: TRect
  ) ;
begin
  if (aIndex >= 0) and (aIndex < Length(FSelections))
     then FSelections[aIndex] := AValue
     else Raise Exception.CreateFmt('Error in SetSelection: Index out of range (%d)', [aIndex]);
end;

constructor TOcrivistPage.Create( pix: PLPix );
begin
  FText := TStringList.Create;
  if pix <> nil
     then SaveToTempfile(pix, GetTempDir);
  SetLength(FSelections, 0);
end;

destructor TOcrivistPage.Destroy;
var
  c: Integer;
begin
  FText.Free;
  if FileExists(FTempFile) then DeleteFile(FTempFile);
  inherited Destroy;
end;

function TOcrivistPage.LoadFromTempfile: PLPix;
begin
  Result := nil;
  if Length(FTempFile)>0
     then if FileExists(FTempFile)
        then Result := pixRead( PChar(FTempFile) );
end;

//NB: function will not work as expected if Ftempfile already exists and different Path is given
procedure TOcrivistPage.SaveToTempfile ( Pix: PLPix; Path: String ) ;
begin
  if Length(Path)=0 then Path := GetTempDir;
  if FTempFile=''
     then FTempFile := GetTempFileName( Path, 'ovp' );
  pixWrite( Pchar(FTempFile), pix, IFF_TIFF_LZW);
end;

procedure TOcrivistPage.AddSelection ( Sel: TRect ) ;
begin
  SetLength(FSelections, Length(FSelections) + 1);
  FSelections[Length(FSelections)-1] := Sel;
end;

procedure TOcrivistPage.DeleteSelection ( aIndex: Integer ) ;
var
  X: LongInt;
begin
  if (aIndex >= 0) and (aIndex < Length(FSelections)) then
     begin
       for X := aIndex to Length(FSelections)-2 do
         FSelections[X] := FSelections[X+1];
       SetLength(FSelections, Length(FSelections)-1);
     end
  else Raise Exception.CreateFmt('Error in DeleteSelection: Index out of range (%d)', [aIndex]);
end;

end.

