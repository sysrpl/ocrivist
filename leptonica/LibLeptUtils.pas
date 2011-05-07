unit LibLeptUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, leptonica, sane, ScanUtils;

type
  TProgressCallback = procedure(progress: Single) of object;


const
  //BUFFERSIZE = 32768;
  BUFFERSIZE = 512000;

function ScaleToBitmap( pix: TLeptPix;  bmp: TBitmap; scalexy: Single ): integer;
function CropPix( pix: TLeptPix; cropRect: TRect ): TLeptPix;
function CropPix( pix: TLeptPix; x, y, w, h: longint ): TLeptPix;
function ScanToPix( h: SANE_Handle; resolution: integer; scanmode: string; progresscb: TProgressCallback ): TLeptPix;

implementation

function ScaleToBitmap ( pix: TLeptPix;  bmp: TBitmap; scalexy: Single ) : integer;
var
  ms: TMemoryStream;
  tempPix: TLeptPix;
  buf: PByteArray;
  bytecount: Integer;
begin
  Result := 1;
  if pix = nil then Exit;
  ms := TMemoryStream.Create;
  tempPix := pixScaleSmooth(pix, scalexy, scalexy);
  if tempPix<>nil then
     try
       WriteLn( pixWriteMem(@buf, @bytecount, tempPix, IFF_BMP) );
       writeln(bytecount);
       ms.Write(buf^[0], bytecount);
       ms.Position := 0;
      if bmp <> nil then
       bmp.LoadFromStream(ms) else Raise Exception.Create('bitmap is nil in ScaleToBitmap');
     finally
       pixDestroy(@tempPix);
       ms.Free;
       Result := 0;
     end;
end;

function CropPix ( pix: TLeptPix; cropRect: TRect ) : TLeptPix;
begin
  Result := CropPix(pix, cropRect.Left, cropRect.Top, cropRect.Right-cropRect.Left+1, cropRect.Bottom-cropRect.Top+1);
end;

function CropPix ( pix: TLeptPix; x, y, w, h: longint ) : TLeptPix;
var
  BoxRect: TLBox;
  pixB: Pointer;
begin
//  writeln( Format('box: %d %d %d %d', [x, y, w, h]) );
  BoxRect := boxCreateValid(x, y, w, h);
  //if BoxRect <> nil then
     pixB := pixClipRectangle( pix, BoxRect, nil);
  if pixB <> nil then
     begin
       pixDestroy(@pix);
       Result := pixB;
     end
  else Result := pix;
  boxDestroy(@BoxRect);
end;

function ScanToPix ( h: SANE_Handle; resolution: integer; scanmode: string; progresscb: TProgressCallback ) : TLeptPix;
var
  fileheader: String;
  status: SANE_Status;
  data: array[0..BUFFERSIZE-1] of char;
  l : SANE_Int;
  par: SANE_Parameters;
  ms: TMemoryStream;
  byteswritten: Cardinal;
  pixeltotal: Integer;
begin
  Result := nil;
     if h<>nil then
        try
          // Set selected resolution
         writeln('open');
          SaneSetOption(h, SANE_NAME_SCAN_RESOLUTION, IntToStr(resolution) );
         writeln('resolution set');
          // set selected scan mode
          SaneSetOption(h, SANE_NAME_SCAN_MODE, scanmode) ;
          writeln('mode set');

{          if Pos('~/', FileNameEdit.Text) = 1
             then filename := GetEnvironmentVariable('HOME') + Copy(FileNameEdit.Text, 2, MaxInt)
             else filename := FileNameEdit.Text;
          f := FileCreate(filename);  }
          writeln('start scan');
          status := sane_start(h);  // start scanning
          ms := TMemoryStream.Create;
          if status = SANE_STATUS_GOOD then
             begin
                if sane_get_parameters(h, @par) = SANE_STATUS_GOOD then // we know the image dimensions and depth
                  SetPNMHeader(par.depth, par.pixels_per_line, par.lines, par.format, fileheader);
                pixeltotal := par.lines * par.pixels_per_line * par.depth div 8;
                if par.format>SANE_FRAME_GRAY then pixeltotal := pixeltotal*3;
                writeln('depth: ', par.depth);
                byteswritten := ms.Write(fileheader[1], Length(fileheader));
                writeln('par format: ', par.format);
                while status = SANE_STATUS_GOOD do
                  begin
                    status := sane_read(h, @data, BUFFERSIZE, @l );    //read data stream from scanner
                    byteswritten := byteswritten + ms.Write(data[0], l);
                    //writeln('byteswritten: ', byteswritten);
                    if Assigned(progresscb) then progresscb( byteswritten / pixeltotal );
                  end;
                data[0] := #0;
                byteswritten := byteswritten + ms.Write(data[0], 1);
             end
          else writeln('Scan failed: ' + sane_strstatus(status));
          writeln('scan done');
          ms.SaveToFile('/tmp/image.pnm');
          writeln('file saved');
          //Result := pixReadMem(ms.Memory, @byteswritten)
          Result := pixRead(Pchar('/tmp/image.pnm'));
          writeln('file loaded');
        finally
          sane_cancel(h);
          ms.Free;
        end;
end;

end.

