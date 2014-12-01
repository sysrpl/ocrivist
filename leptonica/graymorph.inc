{ This file is an automated translation to pascal of graymorph.c              }
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
 *  graymorph.c
 *
 *      Top-level binary morphological operations (van Herk / Gil-Werman)
 *            PIX     *pixErodeGray()
 *            PIX     *pixDilateGray()
 *            PIX     *pixOpenGray()
 *            PIX     *pixCloseGray()
 *
 *      Special operations for 1x3, 3x1 and 3x3 Sels  (direct)
 *            PIX     *pixErodeGray3()
 *            PIX     *pixDilateGray3()
 *            PIX     *pixOpenGray3()
 *            PIX     *pixCloseGray3()
 *
 *
 *      Method: Algorithm by van Herk and Gil and Werman, 1992
 *
 *      Measured speed of the vH/G-W implementation is about 1 output
 *      pixel per 120 PIII clock cycles, for a horizontal or vertical
 *      erosion or dilation.  The computation time doubles for opening
 *      or closing, or for a square SE, as expected, and is independent
 *      of the size of the SE.
 *
 *      A faster implementation can be made directly for brick Sels
 *      of maximum size 3.  We unroll the computation for sets of 8 bytes.
 *      It needs to be called explicitly; the general functions do not
 *      default for the size 3 brick Sels.
 *}

{*-----------------------------------------------------------------*
 *              Top-level gray morphological operations            *
 *-----------------------------------------------------------------*}

{*!
 *  pixErodeGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixErodeGray( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixDilateGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixDilateGray( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixOpenGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixOpenGray( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCloseGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixCloseGray( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-----------------------------------------------------------------*
 *           Special operations for 1x3, 3x1 and 3x3 Sels          *
 *-----------------------------------------------------------------*}

{*!
 *  pixErodeGray3()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *              hsize  (1 or 3)
 *              vsize  (1 or 3)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for 1x3, 3x1 or 3x3 brick sel (all hits)
 *      (2) If hsize = vsize = 1, just returns a copy.
 *      (3) It would be nice not to add a border, but it is required
 *          if we want the same results as from the general case.
 *          We add 4 bytes on the left to speed up the copying, and
 *          8 bytes at the right and bottom to allow unrolling of
 *          the computation of 8 pixels.
 *}
function pixErodeGray3( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixErodeGray3h()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for horizontal 3x1 brick Sel;
 *          also used as the first step for the 3x3 brick Sel.
 *}

{*!
 *  pixErodeGray3v()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for vertical 1x3 brick Sel;
 *          also used as the second step for the 3x3 brick Sel.
 *      (2) Surprisingly, this is faster than setting up the
 *          lineptrs array and accessing into it; e.g.,
 *              val4 = GET_DATA_BYTE(lines8[i + 3], j);
 *}

{*!
 *  pixDilateGray3()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *              hsize  (1 or 3)
 *              vsize  (1 or 3)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for 1x3, 3x1 or 3x3 brick sel (all hits)
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixDilateGray3( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixDilateGray3h()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for horizontal 3x1 brick Sel;
 *          also used as the first step for the 3x3 brick Sel.
 *}

{*!
 *  pixDilateGray3v()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for vertical 1x3 brick Sel;
 *          also used as the second step for the 3x3 brick Sel.
 *}

{*!
 *  pixOpenGray3()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *              hsize  (1 or 3)
 *              vsize  (1 or 3)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for 1x3, 3x1 or 3x3 brick sel (all hits)
 *      (2) If hsize = vsize = 1, just returns a copy.
 *      (3) It would be nice not to add a border, but it is required
 *          to get the same results as for the general case.
 *}
function pixOpenGray3( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCloseGray3()
 *
 *      Input:  pixs (8 bpp, not cmapped)
 *              hsize  (1 or 3)
 *              vsize  (1 or 3)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Special case for 1x3, 3x1 or 3x3 brick sel (all hits)
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixCloseGray3( pixs: PLPix; hsize, vsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


