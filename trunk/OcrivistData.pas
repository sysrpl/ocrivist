unit OcrivistData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, selector, Graphics, ocr, FileUtil;

type

  { TOcrivistPage }

  TOcrivistPage = class(TObject)
  private
    FTitle: string;
    FTempFile: TFilename;
    FThumbnail: TBitmap;
    FSelections: array of TRect;
    FModified: Boolean;
    FPix:PLPix;
    FOCRData: TTesseractPage;
  private
    FActive: Boolean;
    function getOCRText: string;
    function GetPageImage: PLPix;
    function GetSelection ( aIndex: Integer ) : TRect;
    function GetSelectionCount: Integer;
    procedure SetActive ( const AValue: Boolean ) ;
    procedure SetOCRData ( const AValue: TTesseractPage ) ;
    procedure SetPageImage ( const AValue: PLPix ) ;
    procedure SetSelection ( aIndex: Integer ; const AValue: TRect ) ;
    procedure MakeThumbnail;
    function LoadFromFileBackground: PLPix;
    procedure SaveToFileBackground ( Pix: PLPix; Path: String );
  public
    constructor Create( pix: PLPix );
    destructor Destroy; override;
    procedure AddSelection ( Sel: TRect );
    procedure DeleteSelection ( aIndex: Integer );
    function LoadFromFile: PLPix;
    property Text: string read getOCRText;
    property Selection[ aIndex: Integer ]: TRect read GetSelection write SetSelection;
    property SelectionCount: Integer read GetSelectionCount;
    property Title: string read FTitle write FTitle;
    property Modified: Boolean read FModified write FModified;
    property PageImage: PLPix read GetPageImage write SetPageImage;
    property Thumbnail: TBitmap read FThumbnail;
    property Active: Boolean read FActive write SetActive;
    property OCRData: TTesseractPage read FOCRData write SetOCRData;
  end;

  { TOcrivistProject }

  TOcrivistProject = class( TObject )
  private
    FTitle: string;
    FPages: array of TOcrivistPage;
    FFilename: string;
    FViewerScale: Single;
    FWorkFolder: string;
    FcurrentPage: Integer;
    FPageWidth: Integer;
    FPageHeight: Integer;
    function GetCurrentPage: TOcrivistPage;
    function GetPage ( aIndex: Integer ) : TOcrivistPage;
    function GetPageCount: Integer;
    // PutPage assigns a TOcrivistPage to FPages[aIndex]
    procedure PutPage ( aIndex: Integer; const AValue: TOcrivistPage ) ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile( aFileName: TFilename );
    function LoadfromFile( aFileName: TFilename ): integer;
    procedure DeletePage( aIndex: Integer );
    procedure MovePage( sourceIndex, destIndex: Integer );
    procedure AddPage( Pix: PLPix );
    property Pages[aIndex: Integer]: TOcrivistPage read GetPage write PutPage;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FPageWidth write FPageWidth;
    property Height: Integer read FPageHeight write FPageHeight;
    property CurrentPage: TOcrivistPage read GetCurrentPage;
    property ItemIndex: integer read FcurrentPage write FcurrentPage;
    property PageCount: Integer read GetPageCount;
    property Filename: TFilename read FFilename;
    property ViewerScale: Single read FViewerScale write FViewerScale;
  end;

  { TPixFileThread }

  { Use:
    Create with CreateSuspended = true
    Call either SaveToFile or LoadFromFile
    LoadFromFile will place loaded pix at address Pix
  }
  TPixFileThread = class (TThread)
  private
    FFilename: TFilename;
    FOnComplete: TNotifyEvent;
    FPix: PLPix;
    FPixAddr: PPLPix;
    protected
      procedure Execute; override;
    public
      constructor Create ( CreateSuspended: Boolean; const StackSize: SizeUInt =
        DefaultStackSize ) ;
      Procedure SaveToFile( Pix: PLPix; Dest: TFilename );
      Procedure LoadFromFile( Pix: PPLPix; Src: TFilename );
      property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
      property Pix: PLPix read FPix write FPix;
      property Filename: TFilename read FFilename write FFilename;
  end;

implementation

uses LibLeptUtils;

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
  if Length(FPages)>0
     then Result := FPages[FcurrentPage]
     else Result := nil;
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
  FWorkFolder := '/tmp/';
end;

destructor TOcrivistProject.Destroy;
var
  c: Integer;
begin
  for c := Length(FPages)-1 downto 0 do
    TOcrivistPage(FPages[c]).Free;
  inherited Destroy;
end;

procedure TOcrivistProject.Clear;
var
  c: Integer;
begin
  for c := Length(FPages)-1 downto 0 do
    TOcrivistPage(FPages[c]).Free;
  SetLength( FPages, 0 );
  FTitle := 'Untitled';
  FFilename := '';
  FWorkFolder := '/tmp/';
  FcurrentPage := 0;
end;

procedure TOcrivistProject.SaveToFile ( aFileName: TFilename ) ;
var
  F: THandle;
  databuf: string;
  buffer: array of byte;
  tempbuf: PByteArray;
  bytes: Integer;
  page: Integer;
  aPage: TOcrivistPage;
  bytecount: Integer;
  pageImage: PLPix;
  llines: Integer;
  aline: TLineData;
  wwords: Integer;
  memstream: TMemoryStream;
  filesdirectory: String;
  aWord: TWordData;
  sel: Integer;
  aRect: TRect;
  tempTitle: String;
begin
  if FileExists(aFileName)
     then DeleteFile(aFileName);
  tempTitle := ExtractFileNameOnly(aFileName);
  filesdirectory := ChangeFileExt(aFileName, '') + '-files' + DirectorySeparator;

  if not DirectoryExists(filesdirectory)
         then ForceDirectories(filesdirectory);

  F := FileCreate(aFileName);
  if F > 0 then
     try
       databuf := 'OVT   ';
       FileWrite(F, databuf[1], Length(databuf));              //1 - 6 bytes - file identifier - spare bytes are for version info
       FileWrite(F, FcurrentPage, SizeOf(FcurrentPage));       //2 - integer - current page
       FileWrite(F, FPageWidth, SizeOf(FPageWidth));           //3 - Integer - page width
       FileWrite(F, FPageHeight, SizeOf(FPageHeight));         //4 - Integer - page height;
       FileWrite(F, ViewerScale, SizeOf(ViewerScale));         //4a - Single - Viewer setting;
       bytes := Length(tempTitle);
       FileWrite(F, bytes, SizeOf(bytes));                     //5 - Integer - length of string FTitle
       if bytes>0 then
              FileWrite(F, tempTitle[1], Length(tempTitle));         //6 - array of char - string FTitle
       bytes := Length(FPages);
       FileWrite(F, bytes, SizeOf(bytes));                     //7 - Integer - number of pages in project
       for page := 0 to Length(FPages)-1 do
         begin
           aPage := TOcrivistPage(FPages[page]);
           databuf := aPage.Title;
           bytes := Length(databuf);
           FileWrite(F, bytes, SizeOf(bytes));                 //1 - Integer - length of string Title
           if bytes>0 then
              FileWrite(F, databuf[1], bytes);                 //2 - array of char - string Title
           bytes := Length(aPage.Text);
           FileWrite(F, bytes, SizeOf(bytes));                 //3 - Integer - length of string Text
           databuf := aPage.Text;                              //4 - array of char - = string Text
           if bytes>0 then
              FileWrite(F, databuf[1], bytes);
           databuf := ExtractFileName( aPage.FTempFile );
           //databuf := ExtractFileNameOnly( aPage.FTempFile ) + '.tiff';
           bytes := Length(databuf);
           FileWrite(F, bytes, SizeOf(bytes));                 //5 - Integer - length of string FTempFile
           if bytes>0 then
              FileWrite(F, databuf[1], bytes);                 //6 - array of char - = string FtempFile
           if filesdirectory<>ExtractFilePath(aPage.FTempFile) then
              begin
                CopyFile(aPage.FTempFile, filesdirectory + databuf);
                if FWorkFolder='/tmp/'
                   then DeleteFileUTF8(aPage.FTempFile);
                aPage.FTempFile := filesdirectory + databuf;
              end;

           memstream := TMemoryStream.Create;
           try
             aPage.FThumbnail.SaveToStream(memstream);
             bytes := memstream.Size;
             FileWrite(F, bytes, SizeOf(bytes));                 //7 - Integer - length of FThumbnail
             memstream.Position := 0;
             if bytes>0 then
                begin
                  SetLength(buffer, bytes);
                  memstream.Read(buffer[0], bytes);
                  FileWrite(F, buffer[0], bytes);                //8 - FThumbnail data
                end;
           finally
             memstream.Free;
           end;

           if aPage.FOCRData <> nil
              then bytes := aPage.FOCRData.Linecount
              else bytes := 0;
           FileWrite(F, bytes, SizeOf(bytes));                   //9 - Integer - FOCRData exists: TRUE= >0
           if aPage.FOCRData <> nil then
              for llines := 0 to aPage.FOCRData.Linecount-1 do
              begin
                aline := aPage.FOCRData.Lines[llines];
                FileWrite(F, aline.Box, SizeOf(aline.Box));
                FileWrite(F, aline.WordCount, SizeOf(aline.WordCount));
                for wwords := 0 to aline.WordCount-1 do
                    begin
                      databuf := aline.Words[wwords].Text;
                      bytes := Length(databuf);
                      FileWrite(F, bytes, SizeOf(bytes));// Integer - length of aline.Words[wwords].Text
                      if bytes>0 then
                         FileWrite(F, databuf[1], bytes);// - array of char - = string Text
                      aWord := aline.Words[wwords];
                      aWord.Text := '';                  // set pointer to nil
                      Filewrite(F, aWord, SizeOf(aWord));
                     end;
              end;
           bytes := aPage.SelectionCount;
           FileWrite(F, bytes, SizeOf(bytes));           // Integer - number of selections
           for sel := 0 to aPage.SelectionCount-1 do
             begin
               aRect := aPage.Selection[sel];
               FileWrite(F, aRect, SizeOf(aRect));
             end;
           FFilename := aFileName;
           FWorkFolder := filesdirectory;
           FTitle := tempTitle;
         end;
     finally
       FileClose(F);
     end
  else Raise Exception.Create('Unable to save project ' + FileName);
end;

function TOcrivistProject.LoadfromFile ( aFileName: TFilename ) : integer;
var
  F: THandle;
  databuf: array of char;
  strbuf: string;
  tempbuf: PByte;
  bytes: Integer;
  page: Integer;
  aPage: TOcrivistPage;
  bytecount: Integer;
  lline: Integer;
  wwords: Integer;
  aWord: TWordData;
  aLine: TLineData;
  memstream: TMemoryStream;
  sel: Integer;
  aRect: TRect;
begin
  Result := -1;
  Clear;
  F := FileOpen(aFileName, fmOpenRead);
  if F > 0 then
     try
       SetLength(databuf, 6);
       FileRead(F, databuf[0], 6);
       FileRead(F, FcurrentPage, SizeOf(FcurrentPage));       //2 - integer - current page
       FileRead(F, FPageWidth, SizeOf(FPageWidth));           //3 - Integer - page width
       FileRead(F, FPageHeight, SizeOf(FPageHeight));         //4 - Integer - page height;
       FileRead(F, FViewerScale, SizeOf(FViewerScale));       //4a - Single - Viewer setting;
       FileRead(F, bytes, SizeOf(bytes));                     //5 - Integer - length of string FTitle
       SetLength(FTitle, bytes);
       if bytes>0 then
              FileRead(F, FTitle[1], bytes);                  //6 - array of char - string FTitle
       FWorkFolder := ExtractFilePath(aFileName) + FTitle + '-files' + DirectorySeparator;
       FileRead(F, bytes, SizeOf(bytes));                     //7 - Integer - number of pages in project
       SetLength(FPages, bytes);
       for page := 0 to PageCount-1 do
         begin
           aPage := TOcrivistPage.Create(nil);
           aPage.FActive := false;                // do not load image until necessary
           FPages[page] := aPage;
           FileRead(F, bytes, SizeOf(bytes));                 //1 - Integer - length of string Title
           SetLength(apage.FTitle, bytes+1);
           if bytes>0 then
              FileRead(F, apage.FTitle[1], bytes);            //2 - array of char - string Title
           FileRead(F, bytes, SizeOf(bytes));                 //3 - Integer - length of string Text
           if bytes>0 then
              begin
                aPage.FOCRData := TTesseractPage.Create(nil);
                 SetLength(strbuf, bytes);
                 if bytes>0 then
                    FileRead(F, strbuf[1], bytes);                  //4 - array of char - = string Text
                    //writeln('Got Page Text=', strbuf);
                 aPage.OCRData.Text := strbuf;
              end;
           FileRead(F, bytes, SizeOf(bytes));                 //5 - Integer - length of FTempFile
           SetLength(strbuf, bytes);
           if bytes>0 then
              begin
                FileRead(F, strbuf[1], bytes);                //6 - array of char - = string FtempFile
                aPage.FTempFile := FWorkFolder + strbuf;
                if page=FcurrentPage
                   then aPage.PageImage := aPage.LoadFromFileBackground;
              end;

           FileRead(F, bytes, SizeOf(bytes));                 //7 - Integer - size of FThumbnail
           if bytes>0 then
              try
                memstream := TMemoryStream.Create;
                SetLength(databuf, bytes);
                FileRead(F, databuf[0], bytes);               //8 - FThumbnail data
                memstream.Write(databuf[0], bytes);
                SetLength(databuf, 0); // reduce memory used
                memstream.Position := 0;
                aPage.FThumbnail := TBitmap.Create;
                aPage.Thumbnail.LoadFromStream(memstream);
              finally
                memstream.Free;
              end;

           FileRead(F, bytes, SizeOf(bytes));                 //9 - Integer - FOCRData exists: TRUE= >0
           if bytes>0 then
           begin
             aPage.FOCRData.Linecount := bytes;
             for lline := 0 to aPage.FOCRData.Linecount-1 do
               begin
                 aLine.Index := lline;
                 FileRead(F, aLine.Box, SizeOf(aLine.Box));
                 FileRead(F, aLine.WordCount, SizeOf(aLine.WordCount));
   //              writeln('wordcount=', aLine.WordCount);
                 SetLength(aLine.Words, aLine.WordCount);
                 aPage.FOCRData.Lines[lline] := aLine;
                 for wwords := 0 to aLine.WordCount-1 do
                   begin
                     FileRead(F, bytes, SizeOf(bytes));
                     SetLength(strbuf, bytes);
                     if bytes>0
                        then FileRead(F, strbuf[1], bytes);
                     FileRead(F, aWord, SizeOf(aWord));
  //                   writeln('line #', lline, ' Word #', wwords, ' box.top=', aWord.Box.Top);
                     with aPage.FOCRData.Lines[lline] do
                         begin
                           Words[wwords].Start := aWord.Start;
                           Words[wwords].Length := aWord.Length;
                           Words[wwords].Box := aWord.Box;
                           Words[wwords].Confidence := aWord.Confidence;
                           Words[wwords].Text := strbuf;
                         end;
                     writeln('Loaded word: ', aPage.FOCRData.Lines[lline].Words[wwords].Text);
                   end;
               end;
           end;

           FileRead(F, bytes, SizeOf(bytes));         // Integer - number of selections
           for sel := 0 to bytes-1 do
             begin
               FileRead(F, aRect, SizeOf(aRect));
               aPage.AddSelection(aRect);
             end;

         end;
       FFilename := aFileName;
     finally
       FileClose(F);
       Result := 0;
     end
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

procedure TOcrivistProject.MovePage ( sourceIndex, destIndex: Integer ) ;
var
  tempPage: TOcrivistPage;
  x: LongInt;
begin
  tempPage := FPages[sourceIndex];
  if sourceIndex>destIndex then
     begin
       for x := sourceIndex downto destIndex+1 do
         FPages[x] := FPages[x-1];
       Fpages[destIndex] := tempPage;
     end
  else
     begin
       for x := sourceIndex to destIndex-1 do
         FPages[x] := FPages[x+1];
       Fpages[destIndex] := tempPage;
     end;
end;

procedure TOcrivistProject.AddPage ( Pix: PLPix ) ;
var
  P: TOcrivistPage;
begin
  P := TOcrivistPage.Create(Pix);
  if P <> nil then
     begin
       SetLength(FPages, Length(FPages)+1);
       P.SaveToFileBackground(Pix, FWorkFolder);
       FPages[PageCount-1] := P;
       FcurrentPage := PageCount-1;
       FTitle := pixGetText(Pix);
     end;
end;

{ TOcrivistPage }

function TOcrivistPage.GetSelection ( aIndex: Integer ) : TRect;
begin
  if (aIndex >= 0) and (aIndex < Length(FSelections))
     then Result := FSelections[aIndex]
     else Raise Exception.CreateFmt('Error in GetSelection: Index out of range (%d)', [aIndex]);
end;

function TOcrivistPage.getOCRText: string;
begin
  Result := '';
  if Assigned(FOCRData)
     then Result := FOCRData.Text;
end;

function TOcrivistPage.GetPageImage: PLPix;
begin
  writeln('FTempFile: ', FTempFile);
  if FPix = nil
     then if FileExists(FTempFile)
        then FPix := LoadFromFile;
  Result := FPix;
end;

function TOcrivistPage.GetSelectionCount: Integer;
begin
  Result := Length(FSelections);
end;

procedure TOcrivistPage.SetActive ( const AValue: Boolean ) ;
begin
  if FActive = AValue then exit;
  if AValue then
     begin
       FPix := LoadFromFileBackground;
     end
  else
     begin
       pixDestroy(@FPix);
     end;
  FActive := AValue;
end;

procedure TOcrivistPage.SetOCRData ( const AValue: TTesseractPage ) ;
begin
  if AValue=FOCRData then exit;
  if Assigned(FOCRData)
     then FOCRData.Free;
  FOCRData := AValue;
  FModified := true;
end;

procedure TOcrivistPage.SetPageImage ( const AValue: PLPix ) ;
begin
  if AValue<>FPix then
        begin
          writeln('setting new pix in OctrivistPage');
          FPix := AValue;  // Note: TOcrivistPage is not responsible for destroying previous PIX
          MakeThumbnail;
          SaveToFileBackground(Fpix, '');
        end;
end;

procedure TOcrivistPage.SetSelection ( aIndex: Integer ; const AValue: TRect
  ) ;
begin
  if (aIndex >= 0) and (aIndex < Length(FSelections))
     then FSelections[aIndex] := AValue
     else Raise Exception.CreateFmt('Error in SetSelection: Index out of range (%d)', [aIndex]);
end;

procedure TOcrivistPage.MakeThumbnail;
var
  tPix: PLPix;
begin
  tPix := pixScaleToSize(FPix, 0, 100);
  if tPix<>nil then
     begin
       if Assigned(FThumbnail) then FThumbnail.Free;
       FThumbnail := TBitmap.Create;
       ScaleToBitmap(tPix, FThumbnail, 1);
       pixDestroy(@tPix);
     end;
end;

constructor TOcrivistPage.Create( pix: PLPix );
begin
  FThumbnail := nil;
  FActive := true;
  FOCRData := nil;
  FPix := pix;
  if FPix<>nil then
     begin
     FTitle := pixGetText(FPix);
     MakeThumbnail;
     end;
  SetLength(FSelections, 0);
end;

destructor TOcrivistPage.Destroy;
var
  c: Integer;
begin
  if Assigned(FOCRData) then FOCRData.Free;
  if Assigned(FThumbnail) then FThumbnail.Free;
  inherited Destroy;
end;

function TOcrivistPage.LoadFromFileBackground: PLPix;
var
  PixL: PLPix;
  LoadThread: TPixFileThread;
begin
  Result := nil;
  if Length(FTempFile)>0
     then if FileExists(FTempFile) then
        begin
          LoadThread := TPixFileThread.Create(true);
          LoadThread.LoadFromFile(@PixL, FTempFile);
          Result := PixL;
        end;
end;

//NB: function will not work as expected if Ftempfile already exists and different Path is given
procedure TOcrivistPage.SaveToFileBackGround ( Pix: PLPix; Path: String ) ;
var
  SaveThread: TPixFileThread;
begin
  if Length(Path)=0 then Path := GetTempDir;
  if FTempFile=''
     then FTempFile := GetTempFileName( Path, 'ovp' );
  SaveThread := TPixFileThread.Create(true);
  SaveThread.SaveToFile(pix, FTempFile);
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

function TOcrivistPage.LoadFromFile: PLPix;
begin
  FPix := pixRead(PChar(FTempFile));
  FActive := FPix<>nil;
  Result := FPix;
end;

{ TPixFileThread }

procedure TPixFileThread.Execute;
var
  SaveResult: LongInt;
  pixA: PLPix;
begin
  if FPix = nil then
      begin
        writeln('loading pix from ', FFilename, ' in background');
        pixA := pixRead(PChar(FFilename));
        FPixAddr^ := pixA;
        if Pixa<>nil
           then writeln('pix loaded successfully')
           else writeln('failed to load pix');
      end
  else
      begin
        writeln('writing pix to ', FFilename, ' in background');
        SaveResult := pixWrite(PChar(FFilename), FPix, IFF_TIFF_LZW);
        writeln('Result of save is ', SaveResult);
      end;
  if Assigned(FOnComplete)
     then FOnComplete(Self);
end;

constructor TPixFileThread.Create ( CreateSuspended: Boolean;
  const StackSize: SizeUInt ) ;
begin
  FreeOnTerminate := true;
  inherited Create(CreateSuspended);
  Fpix := nil;
  writeln('Created TPixFileThread');
end;

procedure TPixFileThread.SaveToFile ( Pix: PLPix; Dest: TFilename ) ;
begin
  if Pix <> nil then
     begin
       Fpix := Pix;
       FFilename := Dest;
       Execute;
       Free;
     end;
end;

procedure TPixFileThread.LoadFromFile ( Pix: PPLPix; Src: TFilename ) ;
begin
  if Length(Src)>0 then
     if FileExists(Src) then
        begin
          FFilename := Src;
          FPixAddr := Pix;
          Execute;
          Free;
        end;
end;

end.

