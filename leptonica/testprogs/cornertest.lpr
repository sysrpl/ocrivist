program cornertest;

{$mode objfpc}{$H+}

{/*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 -
 -  Redistribution and use in source and binary forms, with or without
 -  modification, are permitted provided that the following conditions
 -  are met:
 -  1. Redistributions of source code must retain the above copyright
 -     notice, this list of conditions and the following disclaimer.
 -  2. Redistributions in binary form must reproduce the above
 -     copyright notice, this list of conditions and the following
 -     disclaimer in the documentation and/or other materials
 -     provided with the distribution.
 -
 -  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 -  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 -  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 -  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL ANY
 -  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 -  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 -  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 -  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 -  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 -  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 -  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *====================================================================*}

{*
 * cornertest.c
 *
 *   e.g., use on witten.tif
 *}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils
  { you can add units after this },
  leptonica;

const
  LINE_SIZE = 9;

var
  pixs: PLPix;
  pta: PPointArray;
  x, y: Integer;
  n: LongInt;
  i: Integer;
  pt: Integer;
begin
  pixs := pixRead('witten.tif');
  // Clean noise in LR corner of witten.tif
  pixSetPixel(pixs, 2252, 3051, 0);
  pixSetPixel(pixs, 2252, 3050, 0);
  pixSetPixel(pixs, 2251, 3050, 0);

  pta := pixFindCornerPixels(pixs);
 // ptaWriteStream(stderr, pta, 1);
  for pt := 0 to pta^.n-1 do
      WriteLn(Format('(%3.0f, %3.0f)', [ pta^.x[pt], pta^.y[pt] ]));

  	// mark corner pixels
  n := ptaGetCount(pta);
  for i := 0 to n-1 do
      begin
        ptaGetIPt(pta, i, @x, @y);
        pixRenderLine(pixs, x - LINE_SIZE, y, x + LINE_SIZE, y, 3,
	              L_FLIP_PIXELS);
        pixRenderLine(pixs, x, y - LINE_SIZE, x, y + LINE_SIZE, 3,
	              L_FLIP_PIXELS);
      end;

  pixWrite('/tmp/cornertest.png', pixs, IFF_PNG);

  pixDestroy(@pixs);
  ptaDestroy(@pta);
  ptaDestroy(@pta);

end.

