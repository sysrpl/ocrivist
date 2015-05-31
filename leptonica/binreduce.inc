{ This file is an automated translation to pascal of binreduce.c              }
{ performed using the Leptonica Library Converter, written by Malcolm Poole   }

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
 *  binreduce.c
 *
 *      Subsampled reduction
 *
 *           PIX    *pixReduceBinary2()
 *
 *      Rank filtered reductions
 *
 *           PIX    *pixReduceRankBinaryCascade()
 *           PIX    *pixReduceRankBinary2()
 *}

{*------------------------------------------------------------------*
 *                       Subsampled reduction                       *
 *------------------------------------------------------------------*}

{*!
 *  pixReduceBinary2()
 *
 *      Input:  pixs
 *              tab (<optional>; if null, a table is made here
 *                   and destroyed before exit)
 *      Return: pixd (2x subsampled), or null on error
 *}
function pixReduceBinary2( pixs: PLPix; intab: PByte ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                   Rank filtered binary reductions                *
 *------------------------------------------------------------------*}

{*!
 *  pixReduceRankBinaryCascade()
 *
 *      Input:  pixs (1 bpp)
 *              level1, ... level 4 (thresholds, in the set {0, 1, 2, 3, 4})
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This performs up to four cascaded 2x rank reductions.
 *      (2) Use level = 0 to truncate the cascade.
 *}
function pixReduceRankBinaryCascade( pixs: PLPix; level1, level2, level3, level4: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixReduceRankBinary2()
 *
 *      Input:  pixs (1 bpp)
 *              level (rank threshold: 1, 2, 3, 4)
 *              intab (<optional>; if null, a table is made here
 *                     and destroyed before exit)
 *      Return: pixd (1 bpp, 2x rank threshold reduced), or null on error
 *
 *  Notes:
 *      (1) pixd is downscaled by 2x from pixs.
 *      (2) The rank threshold specifies the minimum number of ON
 *          pixels in each 2x2 region of pixs that are required to
 *          set the corresponding pixel ON in pixd.
 *}
function pixReduceRankBinary2( pixs: PLPix; level: LongInt; intab: PByte ) : PLPix; cdecl; external LIBLEPT;


