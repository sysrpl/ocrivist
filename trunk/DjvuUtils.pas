unit DjvuUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  Tdjvuencoder = (encC44, encCJB2);

function djvumakepage( sourceimage, dest: TFilename ): Integer;

implementation

function djvumakepage ( sourceimage, dest: TFilename ) : Integer;
var
  Encoder: TProcess;
  workfolder: String;
  cmd: String;
  encformat: Tdjvuencoder;
begin
  Result := -1;
  Encoder := TProcess.Create(nil);
  Encoder.Options := Encoder.Options + [poWaitOnExit];
  workfolder := ExtractFileDir(sourceimage);
  if ExtractFileExt(sourceimage)='pbm'
     then encformat := encCJB2
     else encformat := encC44;
  case encformat of
       encC44:  cmd := 'c44 ';
       encCJB2: cmd := 'cjb2 ';
  end;
  Encoder.CommandLine := cmd + sourceimage + #32 + dest;
  Encoder.Execute;
  Result := Encoder.ExitStatus;
  Encoder.Free;
end;

end.

