{ LibLeptUtils.pas
  Utilities for working with Leptonica's liblept image processing library

  Copyright (C) 2011 Malcolm Poole <mgpoole@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit LibLeptUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, leptonica
  {$IFDEF HAS_LIBSANE}
  , sane, ScanUtils
  {$ENDIF};

type
  TWinBMPFileHeader = packed record
    FileType: Word;     // File type, always 4D42h ("BM")
    FileSize: DWord;     // Size of the file in bytes
    Reserved: Word;    // Always 0
    Reserved2: Word;    // Always 0
    BitmapOffset: DWord; // Starting position of image data in bytes
  end;

  TWin3xBitmapHeader = packed record
    Size: DWord;            // Size of this header in bytes
    Width: LongInt;           // Image width in pixels
    Height: LongInt;          // Image height in pixels
    Planes: Word;          // Number of color planes
    BitsPerPixel: Word;    // Number of bits per pixel
    Compression: DWord;     // Compression methods used
    SizeOfBitmap: DWord;    // Size of bitmap in bytes
    HorzResolution: LongInt;  // Horizontal resolution in pixels per meter
    VertResolution: LongInt;  // Vertical resolution in pixels per meter
    ColorsUsed: DWord;      // Number of colors in the image
    ColorsImportant: DWord; // Minimum number of important colors
  end;


type
  TProgressCallback = procedure(progress: Single) of object;

const
  //BUFFERSIZE = 32768;
  BUFFERSIZE = 512000;

function ScaleToBitmap( pix: PLPix;  bmp: TBitmap; scalexy: Single ): integer;
function CropPix( pix: PLPix; cropRect: TRect ): PLPix;
function CropPix( pix: PLPix; x, y, w, h: longint ): PLPix;
{$IFDEF HAS_LIBSANE}
function ScanToPix( h: SANE_Handle; progresscb: TProgressCallback ): PLPix;
{$ENDIF}
function BoxToRect( aBox: PLBox ): TRect;
function StreamToPix( S: TMemoryStream ): PLPix;
function PixToBitmap( P: PLPix ): TBitmap;
procedure LoadPixToBitmap( P: PLPix; BMP: TBitmap );


implementation

uses FPimage, FPWriteBMP, FPReadTiff;

function ScaleToBitmap ( pix: PLPix;  bmp: TBitmap; scalexy: Single ) : integer;
var
  ms: TMemoryStream;
  tempPix: PLPix;
  buf: PByte;
  bytecount: Integer;
  fpim: TFPMemoryImage;
  writer: TFPWriterBMP;
  Reader: TFPReaderTiff;
begin
  Result := 1;
  if pix = nil then Exit;
  ms := TMemoryStream.Create;
  tempPix := pixScaleSmooth(pix, scalexy, scalexy);
  if tempPix<>nil then
    // pixWriteMem does not work in Windows
    if pixWriteMem(@buf, @bytecount, tempPix, {$IFDEF MSWINDOWS} IFF_TIFF {$ELSE} IFF_BMP {$ENDIF})=0 then
     try
       ms.Write(buf[0], bytecount);
       ms.Position := 0;
       {$IFDEF MSWINDOWS}
       fpim := TFPMemoryImage.create(pixGetWidth(tempPix), pixGetHeight(tempPix));
       Reader := TFPReaderTiff.Create;
       fpim.LoadFromStream(ms, Reader);
       ms.Clear;
       writer := TFPWriterBMP.Create;
       fpim.SaveToStream(ms, writer);
       Reader.Free;
       writer.Free;
       fpim.Free;
       ms.Position := 0;
       {$ENDIF}
      if bmp <> nil then
       bmp.LoadFromStream(ms) else Raise Exception.Create('bitmap is nil in ScaleToBitmap');
     finally
       pixDestroy(@tempPix);
       ms.Free;
       Result := 0;
     end;
end;

function CropPix ( pix: PLPix; cropRect: TRect ) : PLPix;
begin
  Result := CropPix(pix, cropRect.Left, cropRect.Top, cropRect.Right-cropRect.Left+1, cropRect.Bottom-cropRect.Top+1);
end;

function CropPix ( pix: PLPix; x, y, w, h: longint ) : PLPix;
var
  BoxRect: PLBox;
  pixB: PLPix;
begin
//  writeln( Format('box: %d %d %d %d', [x, y, w, h]) );
  pixB := nil;
  BoxRect := boxCreateValid(x, y, w, h);
  if BoxRect <> nil then
     pixB := pixClipRectangle( pix, BoxRect, nil);
  if pixB <> nil then
     begin
       Result := pixB;
//       pixWrite('/tmp/croppedpix.bmp', pixB, IFF_BMP);
     end
  else Result := pix;
  boxDestroy(@BoxRect);
end;

{$IFDEF HAS_LIBSANE}
function ScanToPix ( h: SANE_Handle; progresscb: TProgressCallback ) : PLPix;
var
  fileheader: String;
  status: SANE_Status;
  data:PByte;
  l : SANE_Int;
  par: SANE_Parameters;
  byteswritten: Cardinal;
  pixeltotal: Integer;
  byt: Integer;
  pixImage: PLPix;
begin
  Result := nil;
  data := nil;
     if h<>nil then
        try
          {$IFDEF DEBUG} writeln('start scan'); {$ENDIF}
          status := sane_start(h);  // start scanning
          if status = SANE_STATUS_GOOD then
             try
                {$IFDEF DEBUG} writeln('sane_start=SANE_STATUS_GOOD'); {$ENDIF}
                if Assigned(progresscb) then progresscb( 0 );
                if sane_get_parameters(h, @par) = SANE_STATUS_GOOD then // we know the image dimensions and depth
                  SetPNMHeader(par.depth, par.pixels_per_line, par.lines, par.format, fileheader);
                pixeltotal := par.lines * par.pixels_per_line * par.depth div 8;
                if par.format>SANE_FRAME_GRAY then pixeltotal := pixeltotal*3;
                Getmem(data, Length(fileheader)+pixeltotal+1);
                for byt := 0 to Length(fileheader)-1 do
                    data[byt] := byte(fileheader[byt+1]);
                byteswritten := byt+1;
                {$IFDEF DEBUG} writeln('par format: ', par.format); {$ENDIF}
                while status = SANE_STATUS_GOOD do
                  begin
                    status := sane_read(h, @data[byteswritten], BUFFERSIZE, @l );    //read data stream from scanner
                    byteswritten := byteswritten+l;
                    if Assigned(progresscb) then progresscb( byteswritten / pixeltotal );
                  end;
             finally
               sane_cancel(h);
               {$IFDEF DEBUG} writeln('scan done');  {$ENDIF}
               if status=SANE_STATUS_EOF then
                 begin
                   pixImage := pixReadMem(data, byteswritten);
                   Result := pixImage;
                 end;
             end
          {$IFDEF DEBUG} else writeln('Scan failed: ' + sane_strstatus(status)) {$ENDIF};
        finally
          if data<>nil then Freemem(data);
        end;
end;
{$ENDIF}

function BoxToRect ( aBox: PLBox ) : TRect;
var
  x, y, w, h: Longint;
begin
  Result := Bounds( 0, 0, 0, 0);
  if boxGetGeometry(aBox, @x, @y, @w, @h)=0 then
     Result := Bounds(x, y, w, h);
end;

function StreamToPix ( S: TMemoryStream ) : PLPix;
var
  tempPix: PLPix;
  buf: PByte;
  bytecount: Integer;
begin
  Result := nil;
  if S = nil then Exit;
  tempPix := nil;
  bytecount := S.Size;
  Getmem(buf, bytecount);
  try
    S.Position := 0;
    S.Read(buf[0], bytecount);
    tempPix := pixReadMem(buf, bytecount);
  finally
    Freemem(buf);
    Result := tempPix;
  end;
end;

function PixToBitmap ( P: PLPix ) : TBitmap;
var
  Bitmap: TBitmap;
begin
  Result := nil;
  Bitmap := TBitmap.Create;
  if Bitmap<>nil
      then LoadPixToBitmap(P, Bitmap);
  Result := Bitmap;
end;

procedure LoadPixToBitmap ( P: PLPix; BMP: TBitmap ) ;
var
  aWord: LongWord;
  pB:PByteArray;
  ColorPalette: array of Longword;
  D: Pointer;
  FileHeader: TWinBMPFileHeader;
  BMPHeader: TWin3xBitmapHeader;
  x: Integer;
  w: Integer;
  h: Integer;
  Stream: TMemoryStream;
  line: LongInt;
  pixcount: Integer;
  CMap: PPixCmap;
  nullbyte: byte;
begin
  nullbyte := 0;
  if P=nil then
     begin
       exit;
     end;
  with FileHeader do
       begin
         FileType := 19778;
         FileSize := 0;
         Reserved := 0;
         Reserved2 :=0;
         BitmapOffset := SizeOf(FileHeader) + SizeOf(BMPHeader);
       end;
  with BMPHeader do
       begin
         Size := SizeOf(BMPHeader);
         Width := pixGetWidth(P);
         Height := pixGetHeight(P);
         Planes := 1;
         if pixGetDepth(P)>8 then
            begin
              BitsPerPixel := 24;
              ColorsUsed := 0;
            end
         else
            begin
              BitsPerPixel := pixGetDepth(P);
              ColorsUsed := 1 shl BitsPerPixel;
            end;
         Compression := 0;
         SizeOfBitmap := pixGetHeight(P) * pixGetWidth(P) * BitsPerPixel div 8;
         HorzResolution := 3779 *  pixGetXRes(P) div 100;
         VertResolution := 3779 *  pixGetYRes(P) div 100;
         ColorsImportant := 0;
       end;

  CMap := pixGetColormap(P);
    if (CMap=nil) and not (pixGetDepth(P) in [1,8,32]) then
     begin
       exit;
     end;

  if CMap<>nil then
     begin
       BMPHeader.ColorsUsed := pixcmapGetCount(CMap);
       SetLength(ColorPalette, BMPHeader.ColorsUsed);
       for x := 0 to BMPHeader.ColorsUsed-1 do
         if pixcmapGetColor32(CMap, x, @aWord)=0
             then ColorPalette[x] := aWord shr 8;
     end
  else
  case pixGetDepth(P) of
       1: begin
            SetLength(ColorPalette, BMPHeader.ColorsUsed);
                  ColorPalette[0] := $00FFFFFF ;
                  ColorPalette[1] := $00000000 ;
          end;
       8: begin
            SetLength(ColorPalette, BMPHeader.ColorsUsed);
            for x := 0 to BMPHeader.ColorsUsed-1 do
                  ColorPalette[x] := $00000000+($00010101*x)
          end;
  end;
  FileHeader.BitmapOffset := FileHeader.BitmapOffset + DWord(Length(ColorPalette)*4);

  Stream := TMemoryStream.Create;
  Stream.Write(FileHeader, SizeOf(FileHeader));
  Stream.Write(BMPHeader, SizeOf(BMPHeader));
  if Length(ColorPalette)>0 then Stream.Write(ColorPalette[0], Length(ColorPalette)*4);

  case pixGetDepth(P) of
       1,4,8: for h := pixGetHeight(p)-1 downto 0 do
            begin
              line := 0;
              D := pixGetData(P) + h*pixGetWpl(P);
              for w := 0 to pixGetWpl(p)-1 do
                begin
                  aWord := SwapEndian( LongWord((D + (4*w))^));
                  line := line + Stream.Write(aWord, SizeOf(aWord));
                end;
            end;
       32: for h := pixGetHeight(p)-1 downto 0 do
            begin
              line := 0;
              pB := PByteArray(pixGetData(P) + h*pixGetWpl(P));
              pixcount := 0;
              for w := 0 to pixGetWidth(p)-1 do
                begin
                  line := line + Stream.Write(pB^[pixcount+1], 3);
                  Inc(pixcount, 4);
                end;
              if (line div 4)*4<line then
                 while (line div 4)*4<line do
                   line := line + Stream.Write(nullbyte, 1);
            end;
  end;
  FileHeader.FileSize := Stream.Position;
  Stream.Position := 0;
  Stream.Write(FileHeader, SizeOf(FileHeader));
  Stream.Position := 0;
  BMP.LoadFromStream(Stream);
  Stream.Free;
end;

end.

