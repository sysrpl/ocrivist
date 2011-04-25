unit LibLeptUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, leptonica;

function ScaleToBitmap( pix: TLeptPix;  bmp: TBitmap; scalexy: Single ): integer;

implementation

function ScaleToBitmap ( pix: TLeptPix;  bmp: TBitmap; scalexy: Single ) : integer;
var
 // bmp: TBitmap;
  ms: TMemoryStream;
  tempPix: TLeptPix;
  buf: PByteArray;
  bytecount: Integer;
begin
  Result := 1;
  if pix = nil then Exit;
 // bmp := nil;
  ms := TMemoryStream.Create;
  tempPix := pixScaleSmooth(pix, scalexy, scalexy);
  if tempPix<>nil then
     try
       WriteLn( pixWriteMem(@buf, @bytecount, tempPix, 1) );
       writeln(bytecount);
       ms.Write(buf^[0], bytecount);
       ms.Position := 0;
      // bmp := TBitmap.Create;
      if bmp <> nil then
       bmp.LoadFromStream(ms) else writeln('bmp is nil');
     finally
       pixDestroy(@tempPix);
       ms.Free;
       Result := 0;
     end;
end;

end.

