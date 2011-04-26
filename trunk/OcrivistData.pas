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
  private
    function GetSelection ( Index: Integer ) : TSelector;
    procedure SetSelection ( Index: Integer ; const AValue: TSelector ) ;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromTempfile: TLeptPix;
    procedure SaveToTempfile ( Pix: TLeptPix; Path: String );
    property Text: TStringList read FText write FText;
    property Selection[ Index: Integer ]: TSelector read GetSelection write SetSelection;
  end;

  { TOcrivistProject }

  TOcrivistProject = class( TObject )
  private
    FTitle: string;
    FPages: array of TOcrivistPage;
    FSaveFolder: string;
    FcurrentPage: Integer;
    FPageWidth: Integer;
    FPageHeight: Integer;
    function GetCount: integer;
    function GetPage ( Index: Integer ) : TOcrivistPage;
    procedure PutPage ( Index: Integer; const AValue: TOcrivistPage ) ;
  public
    destructor Destroy; override;
    procedure SaveToFile( FileName: TFilename );
    function LoadfromFile( FileName: TFilename ): integer;
    //procedure DeletePage
    //procedure AddPage
    property Pages[Index: Integer]: TOcrivistPage read GetPage write PutPage;
    property Count: integer read GetCount;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FPageWidth write FPageWidth;
    property Height: Integer read FPageHeight write FPageHeight;
    property CurrentPage: integer read FcurrentPage;
  end;

implementation

{ TOcrivistProject }

function TOcrivistProject.GetCount: integer;
begin
  Result := Length(FPages);
end;

function TOcrivistProject.GetPage ( Index: Integer ) : TOcrivistPage;
begin
  Result := nil;
  if Index >=  0
     then if Index < Length(FPages)
        then Result := FPages[Index];
end;

procedure TOcrivistProject.PutPage ( Index: Integer;
  const AValue: TOcrivistPage ) ;
begin
  if Index>=Length(FPages) then SetLength(FPages, Index);
  FPages[Index] := AValue;
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
begin

end;

function TOcrivistProject.LoadfromFile ( FileName: TFilename ) : integer;
begin
  Result := -1;
end;

{ TOcrivistPage }

function TOcrivistPage.GetSelection ( Index: Integer ) : TSelector;
begin
  Result := nil;
  if Index < 0 then Exit;
  if Index < Length(FSelections) then
     Result := FSelections[Index];
end;

procedure TOcrivistPage.SetSelection ( Index: Integer ; const AValue: TSelector
  ) ;
begin
  if Index>=Length(FSelections) then SetLength(FSelections, Index);
  FSelections[Index] := AValue;
end;

constructor TOcrivistPage.Create;
begin
  FText := TStringList.Create;
end;

destructor TOcrivistPage.Destroy;
var
  c: Integer;
begin
  FText.Free;
  for c := Length( FSelections )-1 downto 0 do
    TSelector(FSelections).Free;
  inherited Destroy;
end;

function TOcrivistPage.LoadFromTempfile: TLeptPix;
begin

end;

procedure TOcrivistPage.SaveToTempfile ( Pix: TLeptPix; Path: String ) ;
begin

end;

end.

