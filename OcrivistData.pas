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
    FSelections: array of TSelector;
    FModified: Boolean;
  private
    function GetSelection ( Index: Integer ) : TSelector;
    procedure SetPageImage ( const AValue: TLeptPix ) ;
    procedure SetSelection ( Index: Integer ; const AValue: TSelector ) ;
  public
    constructor Create( pix: TLeptPix );
    destructor Destroy; override;
    function LoadFromTempfile: TLeptPix;
    procedure SaveToTempfile ( Pix: TLeptPix; Path: String );
    property Text: TStringList read FText write FText;
    property Selection[ Index: Integer ]: TSelector read GetSelection write SetSelection;
    property Title: string read FTitle write FTitle;
    property Modified: Boolean read FModified write FModified;
    property PageImage: TLeptPix read LoadFromTempfile write SetPageImage;
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
    function GetPage ( Index: Integer ) : TOcrivistPage;
    function GetPageCount: Integer;
    procedure PutPage ( Index: Integer; const AValue: TOcrivistPage ) ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile( FileName: TFilename );
    function LoadfromFile( FileName: TFilename ): integer;
    procedure DeletePage( Index: Integer );
    procedure AddPage( Pix: TLeptPix );
    property Pages[Index: Integer]: TOcrivistPage read GetPage write PutPage;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FPageWidth write FPageWidth;
    property Height: Integer read FPageHeight write FPageHeight;
    property CurrentPage: TOcrivistPage read GetCurrentPage;
    property ItemIndex: integer read FcurrentPage write FcurrentPage;
    property PageCount: Integer read GetPageCount;
  end;

implementation

{ TOcrivistProject }

function TOcrivistProject.GetPage ( Index: Integer ) : TOcrivistPage;
begin
  Result := nil;
  if Index >=  0
     then if Index < Length(FPages)
        then Result := FPages[Index];
end;

function TOcrivistProject.GetCurrentPage: TOcrivistPage;
begin
  Result := FPages[FcurrentPage];
end;

function TOcrivistProject.GetPageCount: Integer;
begin
  Result := Length(FPages);
end;

procedure TOcrivistProject.PutPage ( Index: Integer;
  const AValue: TOcrivistPage ) ;
begin
  if (Index>=0) and (Index<PageCount)
     then FPages[Index] := AValue
     else Raise Exception.Create('Page index out of range (' + IntToStr(Index) + ')');
end;

constructor TOcrivistProject.Create;
begin
  FcurrentPage := 0;
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
  pageImage: PLeptPix;
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

procedure TOcrivistProject.DeletePage ( Index: Integer ) ;
var
  x: LongInt;
begin
  if (Index>=0) and (Index<PageCount) then
     begin
       TOcrivistPage(FPages[Index]).Free;
       for x := Index to PageCount-2 do
         FPages[x] := FPages[x+1];
       SetLength(FPages, PageCount-1);
       if FCurrentPage>=Index
          then Dec(FCurrentPage);
       if (FcurrentPage<0) and (PageCount>0) then FcurrentPage := 0;
     end
     else Raise Exception.Create('Page index out of range (' + IntToStr(Index) + ')');
end;

procedure TOcrivistProject.AddPage ( Pix: TLeptPix ) ;
var
  P: TOcrivistPage;
begin
  P := TOcrivistPage.Create(Pix);
  if P <> nil then
     begin
       SetLength(FPages, Length(FPages)+1);
       FPages[PageCount-1] := P;
       FcurrentPage := PageCount;
     end;
end;

{ TOcrivistPage }

function TOcrivistPage.GetSelection ( Index: Integer ) : TSelector;
begin
  Result := nil;
  if Index < 0 then Exit;
  if Index < Length(FSelections) then
     Result := FSelections[Index];
end;

procedure TOcrivistPage.SetPageImage ( const AValue: TLeptPix ) ;
begin
  SaveToTempfile(AValue, '');
end;

procedure TOcrivistPage.SetSelection ( Index: Integer ; const AValue: TSelector
  ) ;
begin
  if Index>=Length(FSelections) then SetLength(FSelections, Index);
  FSelections[Index] := AValue;
end;

constructor TOcrivistPage.Create( pix: TLeptPix );
begin
  FText := TStringList.Create;
  if pix <> nil
     then SaveToTempfile(pix, GetTempDir);
end;

destructor TOcrivistPage.Destroy;
var
  c: Integer;
begin
  FText.Free;
  for c := Length( FSelections )-1 downto 0 do
    TSelector(FSelections).Free;
  if FileExists(FTempFile) then DeleteFile(FTempFile);
  inherited Destroy;
end;

function TOcrivistPage.LoadFromTempfile: TLeptPix;
begin
  Result := nil;
  if Length(FTempFile)>0
     then if FileExists(FTempFile)
        then Result := pixRead( PChar(FTempFile) );
end;

//NB: function will not work as expected if Ftempfile already exists and different Path is given
procedure TOcrivistPage.SaveToTempfile ( Pix: TLeptPix; Path: String ) ;
begin
  if Length(Path)=0 then Path := GetTempDir;
  if FTempFile=''
     then FTempFile := GetTempFileName( Path, 'ovp' );
  pixWrite( Pchar(FTempFile), pix, IFF_TIFF_LZW);
end;

end.

