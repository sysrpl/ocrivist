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
 *  graphics.c                                                    PASCAL
 *
 *      Pta generation for arbitrary shapes built with lines
 *
 *          PTA        *generatePtaLine()
 *          PTA        *generatePtaWideLine()
 *          PTA        *generatePtaBox()
 *          PTA        *generatePtaHashBox()
 *          PTA        *generatePtaBoxa()
 *          PTAA       *generatePtaaBoxa()
 *          PTAA       *generatePtaaHashBoxa()
 *          PTA        *generatePtaPolyline()
 *          PTA        *generatePtaFilledCircle()
 *          PTA        *generatePtaLineFromPt()
 *          l_int32     locatePtRadially()
 *
 *      Pta rendering
 *
 *          l_int32     pixRenderPta()
 *          l_int32     pixRenderPtaArb()
 *          l_int32     pixRenderPtaBlend()
 *
 *      Rendering of arbitrary shapes built with lines
 *
 *          l_int32     pixRenderLine()
 *          l_int32     pixRenderLineArb()
 *          l_int32     pixRenderLineBlend()
 *
 *          l_int32     pixRenderBox()                              X
 *          l_int32     pixRenderBoxArb()                           X
 *          l_int32     pixRenderBoxBlend()                         X
 *
 *          l_int32     pixRenderHashBox()
 *          l_int32     pixRenderHashBoxArb()
 *          l_int32     pixRenderHashBoxBlend()
 *
 *          l_int32     pixRenderBoxa()
 *          l_int32     pixRenderBoxaArb()
 *          l_int32     pixRenderBoxaBlend()
 *
 *          l_int32     pixRenderPolyline()
 *          l_int32     pixRenderPolylineArb()
 *          l_int32     pixRenderPolylineBlend()
 *
 *          l_int32     pixRenderRandomCmapPtaa()
 *
 *      Contour rendering on grayscale images
 *
 *          PIX        *pixRenderContours()
 *          PIX        *fpixAutoRenderContours()
 *          PIX        *fpixRenderContours()
 *
 *  The line rendering functions are relatively crude, but they
 *  get the job done for most simple situations.  We use the pta
 *  as an intermediate data structure.  A pta is generated
 *  for a line.  One of two rendering functions are used to
 *  render this onto a Pix.
 *}

{*------------------------------------------------------------------*
 *           Rendering of arbitrary shapes built with lines         *
 *------------------------------------------------------------------*}

{*!
 *  pixRenderBox()
 *
 *      Input:  pix
 *              box
 *              width  (thickness of box lines)
 *              op  (one of L_SET_PIXELS, L_CLEAR_PIXELS, L_FLIP_PIXELS)
 *      Return: 0 if OK, 1 on error
 *}
function pixRenderBox( pix: PLPix; box: PLBox; width, op: Longint): Longint; cdecl; external LIBLEPT;

{*!
 *  pixRenderBoxArb()
 *
 *      Input:  pix
 *              box
 *              width  (thickness of box lines)
 *              rval, gval, bval
 *      Return: 0 if OK, 1 on error
 *}
function pixRenderBoxArb(pix: PLPix; box: PLBox; width: Longint; rval, gval, bval: word): Longint; cdecl; external LIBLEPT;

{*!
 *  pixRenderBoxBlend()
 *
 *      Input:  pix
 *              box
 *              width  (thickness of box lines)
 *              rval, gval, bval
 *              fract (in [0.0 - 1.0]; complete transparency (no effect)
 *                     if 0.0; no transparency if 1.0)
 *      Return: 0 if OK, 1 on error
 *}
function pixRenderBoxBlend( pix: PLPix; box: PLBox; width: Longint; rval, gval, bval: word; fract: Single): Longint; cdecl; external LIBLEPT;

