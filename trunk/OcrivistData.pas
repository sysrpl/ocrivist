unit OcrivistData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, leptonica, selector, Graphics, ocr, FileUtil, zipper, zstream;

type

  TPageOperation = (opoCrop, opoDeskew, opoRotate);
  TPageOperations = set of TPageOperation;

  { TOcrivistPage }

  TOcrivistPage = class(TObject)
  private
    FTitle: string;
    FImageFile: TFilename;
    FThumbnail: TBitmap;
    FSelections: array of TRect;
    FModified: Boolean;
    FPix:PLPix;
    FOCRData: TTesseractPage;
    FImageSource: string;
    PageOperations: TPageOperations;
  private
    FActive: Boolean;
    FImageID: Integer;
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
    property Filename: TFilename read FImageFile;
    property ImageSource: string read FImageSource write FImageSource;
    property ImageID: Integer read FImageID;
  end;

  { TOcrivistProject }
  TLanguageString = array[1..3] of char;

  TOcrivistProject = class( TObject )
  private
    FLanguage: TLanguageString;
    FOnSaveProgress: TProgressEvent;
    FTitle: string;
    FPages: array of TOcrivistPage;
    FFilename: string;
    FViewerScale: Single;
    FWorkFolder: string;
    FCurrentPage: Integer;
    FPageWidth: Integer;
    FPageHeight: Integer;
    FLoadCount: integer;
    FSaveCount: integer;
    function GetCurrentPage: TOcrivistPage;
    function GetLoadCount: integer;   //TODO: What is LOadCount? Required?
    function GetPage ( aIndex: Integer ) : TOcrivistPage;
    function GetPageCount: Integer;
    // PutPage assigns a TOcrivistPage to FPages[aIndex]
    procedure PutPage ( aIndex: Integer; const AValue: TOcrivistPage ) ;
    procedure DoProgress ( Sender: TObject; const Pct: Double ) ;
    procedure SetItemIndex ( AValue: integer ) ;
  protected
    UnzipFile: TUnZipper;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile( aFileName: TFilename );
    function LoadfromFile( aFileName: TFilename ): integer;
    procedure DeletePage( aIndex: Integer );
    procedure MovePage( sourceIndex, destIndex: Integer );
    procedure AddPage( Pix: PLPix; pageindex: integer );
    function ExtractPage( imageID: Integer): Integer;
    property Pages[aIndex: Integer]: TOcrivistPage read GetPage write PutPage;
    property Title: string read FTitle write FTitle;
    property Width: Integer read FPageWidth write FPageWidth;
    property Height: Integer read FPageHeight write FPageHeight;
    property CurrentPage: TOcrivistPage read GetCurrentPage;
    property ItemIndex: integer read FcurrentPage write SetItemIndex;
    property PageCount: Integer read GetPageCount;
    property Filename: TFilename read FFilename;
    property ViewerScale: Single read FViewerScale write FViewerScale;
    property LoadCount: integer read GetLoadCount;
    property Language: TLanguageString read FLanguage write FLanguage;
    property WorkFolder: string read FWorkFolder write FWorkFolder;
    property OnSaveProgress: TProgressEvent read FOnSaveProgress write FOnSaveProgress;
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

var
  CurrentProject: TOcrivistProject;  // Placing this here allows TOcrivistPages to access it

const

  FILEVERSION = #4;

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

function TOcrivistProject.GetLoadCount: integer;
begin
  Inc(FLoadCount);
  Result := FLoadCount;
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

procedure TOcrivistProject.DoProgress ( Sender: TObject; const Pct: Double ) ;
var
  OverallProgress: Extended;
  allpages: Integer;
begin
  if Pct=0 then Inc(FSaveCount);
  AllPages := PageCount+1;      // allow for project file
  OverallProgress := ((FSaveCount-1)/allpages*100) + (Pct / allpages);
  if Assigned (FOnSaveProgress)
     then FOnSaveProgress(Sender, OverallProgress);
end;

procedure TOcrivistProject.SetItemIndex ( AValue: integer ) ;
var
  LastPage: Integer;
begin
  if FcurrentPage = AValue then Exit;
  if FCurrentPage < 0 then Exit;
  LastPage := FCurrentPage;
  FcurrentPage := AValue;
  Pages[LastPage].Active := false;
end;

constructor TOcrivistProject.Create;
begin
  FcurrentPage := -1;
  FPageWidth := 0;
  FPageHeight := 0;
  FWorkFolder := GetTempDir(true) + 'ocrivist-' + GetEnvironmentVariableUTF8('USER') + DirectorySeparator;
  if DirectoryExistsUTF8(FWorkFolder) then
     begin
       //offer to recover any files in it
     end
  else
     ForceDirectoriesUTF8(FWorkFolder);
  FLoadCount := 0;
end;

destructor TOcrivistProject.Destroy;
var
  c: Integer;
begin
  DeleteDirectory(WorkFolder,True);
  for c := Length(FPages)-1 downto 0 do
    TOcrivistPage(FPages[c]).Free;
  {$IFDEF DEBUG} writeln('***** Destroying TOCRIVISTPROJECT *****');  {$ENDIF}
  if UnzipFile<>nil then UnzipFile.Free;
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
  aWord: TWordData;
  sel: Integer;
  aRect: TRect;
  tempTitle: String;
  SaveZip: TZipper;
  fontcount: Integer;
  x: Integer;
  fontname: String;
begin
  tempTitle := ExtractFileNameOnly(aFileName);
  SaveZip := TZipper.Create;
  SaveZip.OnProgress := @DoProgress;
  SaveZip.FileName :=  aFileName + '.part';
  FSaveCount := 0;
  F := FileCreate(FWorkFolder + 'project');
  if F > 0 then
     try
       databuf := 'OVT'+ FILEVERSION + #0#0;
       FileWrite(F, databuf[1], Length(databuf));              //1 - 6 bytes - file identifier - spare bytes are for version info
       FileWrite(F, FcurrentPage, SizeOf(FcurrentPage));       //2 - integer - current page
       FileWrite(F, FPageWidth, SizeOf(FPageWidth));           //3 - Integer - page width
       FileWrite(F, FPageHeight, SizeOf(FPageHeight));         //4 - Integer - page height;
       FileWrite(F, ViewerScale, SizeOf(ViewerScale));         //4a - Single - Viewer setting;
       bytes := Length(tempTitle);
       FileWrite(F, bytes, SizeOf(bytes));                     //5 - Integer - length of string FTitle
       if bytes>0 then
              FileWrite(F, tempTitle[1], Length(tempTitle));   //6 - array of char - string FTitle
       FileWrite(F, FLanguage[1], 3);                          //6a - Last language used for OCR
       if FLoadCount<PageCount then FLoadCount := PageCount;   // should be unnecessary in practoce
       Filewrite(F, FLoadCount, SizeOf(FLoadCount));           //6b - Unique number for page image filenames
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
           bytes := Length(aPage.FImageSource);
           FileWrite(F, bytes, SizeOf(bytes));                 //2a - Integer - length of string FImageSource
           databuf := aPage.FImageSource;                      //2b - array of char - = string Text
           if bytes>0 then
              FileWrite(F, databuf[1], bytes);
           bytes := Length(aPage.Text);                        //3 - Integer - length of string Text
           FileWrite(F, bytes, SizeOf(bytes));                 //    (indicates that OCRData exists for LoadFromFile)
           if aPage.FImageID<0 then aPage.FImageID := page;    //TODO: should not be necessary
           FileWrite(F, aPage.FImageID, SizeOf(aPage.FImageID)); //4 - integer - Image ID
           databuf := ExtractFileName( aPage.FImageFile );
           bytes := Length(databuf);
           FileWrite(F, bytes, SizeOf(bytes));                 //5 - Integer - length of string FTempFile
           if bytes>0 then
              FileWrite(F, databuf[1], bytes);                 //6 - array of char - = string FtempFile
           SaveZip.Entries.AddFileEntry(aPage.FImageFile, Format('%.3d', [aPage.FImageID]) + '.tif');
           SaveZip.Entries[SaveZip.Entries.Count-1].CompressionLevel := clnone;
           if not FileExistsUTF8(aPage.FImageFile)
              then ExtractPage(aPage.FImageID);
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
              begin
                //Font management added for file version 4
                fontcount := aPage.FOCRData.Fonts.Count;
                FileWrite(F, fontcount, SizeOf(fontcount));      //9a - Integer - number of fonts detected on page
                for x := 0 to fontcount-1 do
                  begin
                    fontname := aPage.FOCRData.Fonts.Strings[x];
                    bytes := Length(fontname);
                    Filewrite(F, bytes, SizeOf(bytes));          //9b - Integer - length of font name
                    if bytes>0
                       then Filewrite(F, fontname[1], bytes); //9c - array of char - font name
                  end;
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
              end;
           bytes := aPage.SelectionCount;
           FileWrite(F, bytes, SizeOf(bytes));           // Integer - number of selections
           for sel := 0 to aPage.SelectionCount-1 do
             begin
               aRect := aPage.Selection[sel];
               FileWrite(F, aRect, SizeOf(aRect));
             end;
           FFilename := aFileName;
           FTitle := tempTitle;
         end;
     finally
       FileClose(F);
     end
  else Raise Exception.Create('Unable to save project ' + FileName);
  SaveZip.Entries.AddFileEntry(FWorkFolder + 'project', 'project');
  try
    SaveZip.ZipAllFiles;
    if FileExistsUTF8(aFileName)
       then DeleteFileUTF8(aFileName);
    RenameFileUTF8(aFileName + '.part', aFileName);
  finally
   SaveZip.Free;
  end;
end;

function TOcrivistProject.LoadfromFile ( aFileName: TFilename ) : integer;
var
  F: THandle;
  thisfileversion: byte;
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
  aWordV3: TWordDataV3;
  aLine: TLineData;
  memstream: TMemoryStream;
  sel: Integer;
  aRect: TRect;
  sl: TStringList;
  fontcount: Integer;
  fontname: String;
  x: Integer;
begin
  Result := -1;
  if UnzipFile<>nil then FreeAndNil(UnzipFile);
  //TODO: check for modifications and prompt to save
  UnzipFile := TUnZipper.Create;
  UnzipFile.FileName := aFileName;
  UnzipFile.OutputPath := FWorkFolder;
  sl := TStringList.Create;
  sl.Add('project');
  UnzipFile.UnZipFiles(sl);
  sl.Free;
  //TODO: unzip to MemoryStream and parse from there
  Clear;
  F := FileOpen(FWorkFolder + 'project', fmOpenRead);
  if F > 0 then
     try
       SetLength(databuf, 6);
       FileRead(F, databuf[0], 3);                            // file header - should be 'OVP'
       FileRead(F, thisfileversion, 1);
       FileRead(F, databuf[0], 2);                            // spare bytes
       FileRead(F, FcurrentPage, SizeOf(FcurrentPage));       //2 - integer - current page
       FileRead(F, FPageWidth, SizeOf(FPageWidth));           //3 - Integer - page width
       FileRead(F, FPageHeight, SizeOf(FPageHeight));         //4 - Integer - page height;
       FileRead(F, FViewerScale, SizeOf(FViewerScale));       //4a - Single - Viewer setting;
       FileRead(F, bytes, SizeOf(bytes));                     //5 - Integer - length of string FTitle
       SetLength(FTitle, bytes);
       if bytes>0 then
              FileRead(F, FTitle[1], bytes);                  //6 - array of char - string FTitle
       if thisfileversion>2 then
          begin
            FileRead(F, FLanguage[1], 3);                          //6a - Last language used for OCR
            FileRead(F, FLoadCount, SizeOf(FLoadCount));           //6b - Unique number for page image filenames
          end;
       FileRead(F, bytes, SizeOf(bytes));                     //7 - Integer - number of pages in project
       SetLength(FPages, bytes);
       for page := 0 to PageCount-1 do
         begin
           aPage := TOcrivistPage.Create(nil);
           aPage.FActive := false;                // do not load image until necessary
           FPages[page] := aPage;
           FileRead(F, bytes, SizeOf(bytes));                 //1 - Integer - length of string Title
           SetLength(apage.FTitle, bytes);
           if bytes>0 then
           FileRead(F, apage.FTitle[1], bytes);            //2 - array of char - string Title
           FileRead(F, bytes, SizeOf(bytes));                 //2a - Integer - length of string FImageSource
           SetLength(apage.FImageSource, bytes);
           if bytes>0 then
              FileRead(F, apage.FImageSource[1], bytes);      //2b - array of char - string FImageSource

           FileRead(F, bytes, SizeOf(bytes));                 //3 - Integer - length of string Text
           if bytes>0 then
              begin
                aPage.FOCRData := TTesseractPage.Create(nil); // TODO: initialise language and datapath
                if thisfileversion<2 then
                   begin
                     SetLength(strbuf, bytes);
                     if bytes>0 then
                        FileRead(F, strbuf[1], bytes);                  //4 - array of char - = string Text
                   end;
              end;
           if thisfileversion>2
              then FileRead(F, aPage.FImageID, SizeOf(aPage.FImageID)); //4 - integer - Image ID
           FileRead(F, bytes, SizeOf(bytes));                 //5 - Integer - length of FTempFile
           SetLength(strbuf, bytes);
           if bytes>0 then
              begin
                FileRead(F, strbuf[1], bytes);                //6 - array of char - = string FtempFile
                aPage.FImageFile := FWorkFolder + Format('%.3d', [aPage.FImageID]) + '.tif';
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
             if thisfileversion>3 then
                begin
                  FileRead(F, fontcount, SizeOf(fontcount));  //9a - Integer - number of fonts detected on page
                  for x := 1 to fontcount do
                    begin
                      FileRead(F, bytes, SizeOf(bytes));
                      SetLength(fontname, bytes);            //9b - Integer - length of font name
                      FileRead(F, fontname[1], bytes);       //9c - array of char - font name
                      aPage.FOCRData.Fonts.Add(fontname);
                    end;
                end;
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
                     if thisfileversion<4 then
                        begin
                          FileRead(F, aWord, SizeOf(aWordV3));
                          aWord.FontID := -1;
                        end
                     else FileRead(F, aWord, SizeOf(aWord));
//                     {$IFDEF DEBUG} writeln('line #', lline, ' Word #', wwords, ' box.top=', aWord.Box.Top, ' fontsize: ', aWord.FontSize); {$ENDIF}
                     with aPage.FOCRData.Lines[lline] do
                         begin
                           Words[wwords].Start := aWord.Start;
                           Words[wwords].Length := aWord.Length;
                           Words[wwords].Box := aWord.Box;
                           Words[wwords].Confidence := aWord.Confidence;
                           Words[wwords].FontID := aWord.FontID;
                           Words[wwords].FontSize := aword.FontSize;
                           Words[wwords].FontFlags := aWord.FontFlags;
                           Words[wwords].isNumeric := aWord.isNumeric;
                           Words[wwords].Text := strbuf;
                         end;
                     {$IFDEF DEBUG} writeln('Loaded word: ', aPage.FOCRData.Lines[lline].Words[wwords].Text); {$ENDIF}
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

procedure TOcrivistProject.AddPage ( Pix: PLPix; pageindex: integer  ) ;
var
  P: TOcrivistPage;
  pg: Integer;
begin
  P := TOcrivistPage.Create(Pix);
  if P <> nil then
     begin
       SetLength(FPages, Length(FPages)+1);
       P.FImageID := LoadCount;
       P.SaveToFileBackground(Pix, FWorkFolder);
       for pg := PageCount-2 downto pageindex do
         FPages[pg+1] := FPages[pg];
       FPages[pageindex] := P;
       FcurrentPage := pageindex;
       FTitle := pixGetText(Pix);
     end;
end;

function TOcrivistProject.ExtractPage ( imageID: Integer ) : Integer;
var
  sl: TStringList;
  pageimage: String;
begin
  Result := -1;
  pageimage := Format('%.3d', [ImageID]) + '.tif';
  if FileExistsUTF8(pageimage) then begin Result := 0; Exit; end;
  sl := TStringList.Create;
  try
    sl.Add(pageimage);
    UnzipFile.UnZipFiles(sl);
    if FileExistsUTF8(pageimage)
       then Result := 0;
  finally
    sl.Free;
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
  Result := 'Helloooo!';
  {if Assigned(FOCRData)
     then Result := FOCRData.Text;}
end;

function TOcrivistPage.GetPageImage: PLPix;
begin
  {$IFDEF DEBUG}
  writeln('GetPageImage FImageFile: ', FImageFile);
  {$ENDIF}
  if FPix = nil
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
       FPix := nil;
       {$IFDEF DEBUG} writeln('Pix for page imageID ', FImageID, ' destroyed'); {$ENDIF}
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
          {$IFDEF DEBUG} writeln('setting new pix in OctrivistPage'); {$ENDIF}
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
  FImageID := -1;
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
  if Length(FImageFile)>0
     then if FileExists(FImageFile) then
        begin
          LoadThread := TPixFileThread.Create(true);
          LoadThread.LoadFromFile(@PixL, FImageFile);
          Result := PixL;
          FActive := true;
          {$IFDEF DEBUG} writeln('Image for page index ', FImageID, ' loaded in background'); {$ENDIF}
        end;
end;

//NB: function will not work as expected if Ftempfile already exists and different Path is given
procedure TOcrivistPage.SaveToFileBackGround ( Pix: PLPix; Path: String ) ;
var
  SaveThread: TPixFileThread;
begin
  if Length(Path)=0 then Path := CurrentProject.WorkFolder;
  if FImageFile=''
     then FImageFile := Path + Format( 'image%.3d.tif', [ImageID] );
  SaveThread := TPixFileThread.Create(true);
  SaveThread.SaveToFile(pix, FImageFile);
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
  if not FileExistsUTF8(FImageFile)
     then CurrentProject.ExtractPage(FImageID);
  FPix := pixRead(PChar(FImageFile));
  FActive := FPix<>nil;
  Result := FPix;
  {$IFDEF DEBUG} if Result<>nil then writeln('Pix for page index ', FImageID, ' loaded'); {$ENDIF}
end;

{ TPixFileThread }

procedure TPixFileThread.Execute;
var
  SaveResult: LongInt;
  pixA: PLPix;
begin
  if FPix = nil then
      begin
        {$IFDEF DEBUG}
        writeln('loading pix from ', FFilename, ' in background');
        {$ENDIF}
        pixA := pixRead(PChar(FFilename));
        FPixAddr^ := pixA;
        {$IFDEF DEBUG}
        if Pixa<>nil
           then writeln('pix loaded successfully')
           else writeln('failed to load pix');
        {$ENDIF}
      end
  else
      begin
        {$IFDEF DEBUG}
        writeln('writing pix to ', FFilename, ' in background');
        {$ENDIF}
        SaveResult := pixWrite(PChar(FFilename), FPix, IFF_TIFF_LZW);
        {$IFDEF DEBUG}
        writeln('Result of save is ', SaveResult);
        {$ENDIF}
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
  {$IFDEF DEBUG}
  writeln('Created TPixFileThread');
  {$ENDIF}
end;

procedure TPixFileThread.SaveToFile ( Pix: PLPix; Dest: TFilename ) ;
begin
  if Pix <> nil then
     begin
       Fpix := Pix;
       FFilename := Dest;
       Start;
     end;
end;

procedure TPixFileThread.LoadFromFile ( Pix: PPLPix; Src: TFilename ) ;
begin
  if Length(Src)>0 then
     if FileExists(Src) then
        begin
          FFilename := Src;
          FPixAddr := Pix;
          Start;
        end;
end;

end.

