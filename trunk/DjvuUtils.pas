unit DjvuUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  Tdjvuencoder = (encC44, encCJB2);

  TDjvuPageInfo = record
    Width: Integer;
    Height: Integer;
    Res: Integer;
    HasText: Boolean;
  end;

  TDjvuDocInfo = record
    Filename: TFilename;
    PageCount: Integer;
    Pages: array of TDjvuPageInfo;
  end;

const
  READ_BYTES = 2048;

var
    DjVuPath: string;

function djvumakepage( sourceimage, dest: TFilename; dsedtext: PChar; resolution: Integer = 300 ): Integer;
function djvuaddpage( docname, pagename: TFilename ): Integer;
function djvuGetDocInfo( doc: TFilename ): TDjvuDocInfo;
function djvuExtractPage ( doc, dest: TFilename; pageindex: Integer ) : Boolean;

implementation

function djvumakepage ( sourceimage, dest: TFilename; dsedtext: PChar; resolution: Integer = 300 ): Integer;
var
  Encoder: TProcess;
  cmd: String;
  encformat: Tdjvuencoder;
begin
  Result := -1;
  Encoder := TProcess.Create(nil);
  Encoder.Options := Encoder.Options + [poWaitOnExit, poNoConsole];
  if ExtractFileExt(sourceimage)='.pnm'
     then encformat := encCJB2
     else encformat := encC44;
  case encformat of
       encC44:  cmd := '"' + DjVuPath + 'c44" ';
       encCJB2: cmd := '"' + DjVuPath + 'cjb2" ';
  end;
  Encoder.CommandLine := cmd + '-dpi ' + IntToStr(resolution) + #32 + sourceimage + #32 + dest;
  Encoder.Execute;
  Result := Encoder.ExitStatus;
  if dsedtext<> nil then
     begin
       Encoder.CommandLine := '"' + DjVuPath + 'djvused" ' + dest + ' -f ' + dsedtext + ' -s';
       Encoder.Execute;
       Result := Encoder.ExitStatus;
     end;
  Encoder.Free;
end;

function djvuaddpage ( docname, pagename: TFilename ) : Integer;
var
  Encoder: TProcess;
  cmd: String;
begin
  Result := -1;
  Encoder := TProcess.Create(nil);
  Encoder.Options := Encoder.Options + [poWaitOnExit, poNoConsole];
  if FileExists(docname)
     then cmd := '"' + DjVuPath + 'djvm" -i "' + docname + '" "' + pagename + '"'
     else cmd := '"' + DjVuPath + 'djvm" -c "' + docname + '" "' + pagename + '"';
  Encoder.CommandLine := cmd;
  Encoder.Execute;
  Result := Encoder.ExitStatus;
  Encoder.Free;
end;

function djvuGetDocInfo ( doc: TFilename ) : TDjvuDocInfo;
var
  dc: TDjvuDocInfo;
  pg: TDjvuPageInfo;
  Decoder: TProcess;
  cmd: String;
  M: TMemoryStream;
  BytesRead: Integer;
  n: LongInt;
  DocOutput: TStringList;
  p, q, x: LongInt;

begin
  if not FileExists(doc)
     then raise Exception.Create(doc + ' not found in djvuGetDocInfo');
  M := TMemoryStream.Create;
  BytesRead := 0;
  Decoder := TProcess.Create(nil);
  Decoder.Options := Decoder.Options + [poUsePipes, poNoConsole];
  Decoder.CommandLine := '"' + DjVuPath + 'djvudump" "' + doc + '"';
  Decoder.Execute;
  while Decoder.Running do
    begin
     M.SetSize(BytesRead + READ_BYTES);
     n := Decoder.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0 then
       Inc(BytesRead, n)
     else begin
         // no data, wait 100 ms
         Sleep(100);
        end;
    end;
  // read last part
  repeat
    M.SetSize(BytesRead + READ_BYTES);
    n := Decoder.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if n > 0
       then Inc(BytesRead, n);
  until n <= 0;
  Decoder.Free;
  DocOutput := TStringList.Create;
  DocOutput.LoadFromStream(M);
  M.Free;
  n := 0;
  while ( n<DocOutput.Count ) and (Pos('DIRM [', DocOutput.Strings[n]) = 0)  do
        Inc(n);
  p := Pos('DIRM [', DocOutput.Strings[n]);
//  writeln('p=', p, ' n=', n);
  if p > 0 then
    begin
      x := 0;
      q := Pos(' pages', DocOutput.Strings[n]);
//      writeln('q=', q);
      p := q-1;
      while (DocOutput.Strings[n][p] in ['0'..'9']) do Dec(p);
//      writeln('p=', p);
//writeln      (Copy(DocOutput.Strings[n], p+1, q-p));
      if not TryStrToInt( Copy(DocOutput.Strings[n], p+1, q-p-1), x )
         then {$IFDEF DEBUG} writeln('Error in djvuGetDocInfo retrieving pagecount: '+#10+#32+DocOutput.Strings[n] ) {$ENDIF};
    end;
  dc.PageCount := x;
//  writeln('dc.PageCount=', dc.PageCount);
  Result := dc;
  DocOutput.Free;
end;

function djvuExtractPage ( doc, dest: TFilename; pageindex: Integer ) : Boolean;
var
  Decoder: TProcess;
begin
  Result := false;
  if not FileExists(doc)
     then raise Exception.Create(doc + ' not found in djvuExtractPage');
  Decoder := TProcess.Create(nil);
  Decoder.Options := Decoder.Options + [poWaitOnExit, poNoConsole];
  Decoder.CommandLine := '"' + DjVuPath + 'ddjvu" -format=tif -page=' + IntToStr(pageindex) + ' -1 "' + doc + '" ' + dest;
  Decoder.Execute;
  Result := Decoder.ExitStatus=0;
  Decoder.Free;
end;

end.

