program otsutest2;

{$mode objfpc}{$H+}

{*====================================================================*
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
 *   otsutest2.lpr
 *   translated to pascal from otsutest.c by Malcolm Poole, 2014
 *
 *   This demonstrates the usefulness of the modified version of Otsu
 *   for thresholding an image that doesn't have a well-defined
 *   background color.
 *
 *   Standard Otsu binarization is done with scorefract = 0.0, which
 *   returns the threshold at the maximum value of the score.  However.
 *   this value is up on the shoulder of the background, and its
 *   use causes some of the dark background to be binarized as foreground.
 *
 *   Using the modified Otsu with scorefract = 0.1 returns a threshold
 *   at the lowest value of this histogram such that the score
 *   is at least 0.9 times the maximum value of the score.  This allows
 *   the threshold to be taken in the histogram minimum between
 *   the fg and bg peaks, producing a much cleaner binarization.
 *}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes , sysutils
  { you can add units after this },
  leptonica;

var
  textstr: string;
  pixs: PLPix;
  pixg: PLPix;
  pixb: PLPix;
  i: Integer;
  pixa: PPixArray;
  scorefract: Single;
  thresh, fgval, bgval: Longint;
  pixp: PLPix;
  pixt1: PLPix;
  pixt2: PLPix;
  bmf: L_BMF;
begin
    pixs := pixRead('1555-7.jpg');
    pixg := pixConvertTo8(pixs, 0);
    bmf := bmfCreate('fonts', 8);
    for i := 0 to 2 do
        begin
          pixa := pixaCreate(3);
          scorefract := 0.1 * i;
          pixOtsuAdaptiveThreshold(pixg, 2000, 2000, 0, 0, scorefract,
                                   nil, @pixb);
          pixSaveTiledOutline(pixb, pixa, 2, 1, 20, 2, 32);
          pixSplitDistributionFgBg(pixg, scorefract, 1, @thresh, @fgval, @bgval, 1);
          writeln(Format('thresh = %d, fgval = %d, bgval = %d', [thresh, fgval,
                 bgval]));

          // Give gnuplot time to write out the plot

          Sleep(1000);

          pixp := pixRead('/tmp/histplot.png');
          pixSaveTiled(pixp, pixa, 1, 0, 20, 1);
          pixt1 := pixaDisplay(pixa, 0, 0);
          textstr :=
               Format('Scorefract = %3.1f ........... Thresh = %d', [scorefract, thresh]);
          pixt2 := pixAddSingleTextblock(pixt1, bmf, PChar(textstr), $00ff0000,
                                        L_ADD_BELOW, nil);
//          pixDisplay(pixt2, 100, 100);
          textstr := Format('/tmp/otsu.%d.png',[ i ]);
          pixWrite(PChar(textstr), pixt2, IFF_PNG);
          pixDestroy(@pixb);
          pixDestroy(@pixp);
          pixDestroy(@pixt1);
          pixDestroy(@pixt2);
          pixaDestroy(@pixa);
        end;


end.

