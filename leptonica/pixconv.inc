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
 *====================================================================*/

/*
 *  pixconv.c                                                              PASCAL
 *
 *      These functions convert between images of different types
 *      without scaling.
 *
 *      Conversion from 8 bpp grayscale to 1, 2, 4 and 8 bpp
 *           PIX        *pixThreshold8()
 *
 *      Conversion from colormap to full color or grayscale
 *           PIX        *pixRemoveColormap()                                  X
 *
 *      Add colormap losslessly (8 to 8)
 *           l_int32     pixAddGrayColormap8()
 *           PIX        *pixAddMinimalGrayColormap8()
 *
 *      Conversion from RGB color to grayscale
 *           PIX        *pixConvertRGBToLuminance()                           X
 *           PIX        *pixConvertRGBToGray()                                X
 *           PIX        *pixConvertRGBToGrayFast()                            X
 *           PIX        *pixConvertRGBToGrayMinMax()
 *
 *      Conversion from grayscale to colormap
 *           PIX        *pixConvertGrayToColormap()  -- 2, 4, 8 bpp
 *           PIX        *pixConvertGrayToColormap8()  -- 8 bpp only
 *
 *      Colorizing conversion from grayscale to color
 *           PIX        *pixColorizeGray()  -- 8 bpp or cmapped
 *
 *      Conversion from RGB color to colormap
 *           PIX        *pixConvertRGBToColormap()
 *
 *      Quantization for relatively small number of colors in source
 *           l_int32     pixQuantizeIfFewColors()
 *
 *      Conversion from 16 bpp to 8 bpp
 *           PIX        *pixConvert16To8()
 *
 *      Conversion from grayscale to false color
 *           PIX        *pixConvertGrayToFalseColor()
 *
 *      Unpacking conversion from 1 bpp to 2, 4, 8, 16 and 32 bpp
 *           PIX        *pixUnpackBinary()
 *           PIX        *pixConvert1To16()
 *           PIX        *pixConvert1To32()
 *
 *      Unpacking conversion from 1 bpp to 2 bpp
 *           PIX        *pixConvert1To2Cmap()
 *           PIX        *pixConvert1To2()
 *
 *      Unpacking conversion from 1 bpp to 4 bpp
 *           PIX        *pixConvert1To4Cmap()
 *           PIX        *pixConvert1To4()
 *
 *      Unpacking conversion from 1, 2 and 4 bpp to 8 bpp
 *           PIX        *pixConvert1To8()
 *           PIX        *pixConvert2To8()
 *           PIX        *pixConvert4To8()
 *
 *      Unpacking conversion from 8 bpp to 16 bpp
 *           PIX        *pixConvert8To16()
 *
 *      Top-level conversion to 1 bpp
 *           PIX        *pixConvertTo1()
 *           PIX        *pixConvertTo1BySampling()
 *
 *      Top-level conversion to 8 bpp
 *           PIX        *pixConvertTo8()                                      X
 *           PIX        *pixConvertTo8BySampling()
 *
 *      Top-level conversion to 16 bpp
 *           PIX        *pixConvertTo16()
 *
 *      Top-level conversion to 32 bpp (RGB)
 *           PIX        *pixConvertTo32()   ***                               X
 *           PIX        *pixConvertTo32BySampling()   ***
 *           PIX        *pixConvert8To32()  ***
 *
 *      Top-level conversion to 8 or 32 bpp, without colormap
 *           PIX        *pixConvertTo8Or32
 *
 *      Conversion between 24 bpp and 32 bpp rgb
 *           PIX        *pixConvert24To32()
 *           PIX        *pixConvert32To24()
 *
 *      Lossless depth conversion (unpacking)
 *           PIX        *pixConvertLossless()
 *
 *      Conversion for printing in PostScript
 *           PIX        *pixConvertForPSWrap()
 *
 *      Scaling conversion to subpixel RGB
 *           PIX        *pixConvertToSubpixelRGB()
 *           PIX        *pixConvertGrayToSubpixelRGB()
 *           PIX        *pixConvertColorToSubpixelRGB()
 *
 *      *** indicates implicit assumption about RGB component ordering
 *}


{*-------------------------------------------------------------*
 *               Conversion from colormapped pix               *
 *-------------------------------------------------------------*/
/*!
 *  pixRemoveColormap()
 *
 *      Input:  pixs (see restrictions below)
 *              type (REMOVE_CMAP_TO_BINARY,
 *                    REMOVE_CMAP_TO_GRAYSCALE,
 *                    REMOVE_CMAP_TO_FULL_COLOR,
 *                    REMOVE_CMAP_BASED_ON_SRC)
 *      Return: new pix, or null on error
 *
 *  Notes:
 *      (1) If there is no colormap, a clone is returned.
 *      (2) Otherwise, the input pixs is restricted to 1, 2, 4 or 8 bpp.
 *      (3) Use REMOVE_CMAP_TO_BINARY only on 1 bpp pix.
 *      (4) For grayscale conversion from RGB, use a weighted average
 *          of RGB values, and always return an 8 bpp pix, regardless
 *          of whether the input pixs depth is 2, 4 or 8 bpp.
 *}
function pixRemoveColormap( pixs: PLPix; newtype: Longint ): PLPix; cdecl; external LIBLEPT;

{*-------------------------------------------------------------*
 *            Conversion from RGB color to grayscale           *
 *-------------------------------------------------------------*/
/*!
 *  pixConvertRGBToLuminance()
 *
 *      Input:  pix (32 bpp RGB)
 *      Return: 8 bpp pix, or null on error
 *
 *  Notes:
 *      (1) Use a standard luminance conversion.
 *}
function pixConvertRGBToLuminance( pixs: PLPix ): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixConvertRGBToGray()
 *
 *      Input:  pix (32 bpp RGB)
 *              rwt, gwt, bwt  (non-negative; these should add to 1.0,
 *                              or use 0.0 for default)
 *      Return: 8 bpp pix, or null on error
 *
 *  Notes:
 *      (1) Use a weighted average of the RGB values.
 *}
function pixConvertRGBToGray( pixs: PLPix; rwt, gwt, bwt: Single ): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixConvertRGBToGrayFast()
 *
 *      Input:  pix (32 bpp RGB)
 *      Return: 8 bpp pix, or null on error
 *
 *  Notes:
 *      (1) This function should be used if speed of conversion
 *          is paramount, and the green channel can be used as
 *          a fair representative of the RGB intensity.  It is
 *          several times faster than pixConvertRGBToGray().
 *      (2) To combine RGB to gray conversion with subsampling,
 *          use pixScaleRGBToGrayFast() instead.
 *}
function pixConvertRGBToGrayFast( pixs: PLPix ): PLPix; cdecl; external LIBLEPT;

{*---------------------------------------------------------------------------*
 *                    Top-level conversion to 32 bpp                         *
 *---------------------------------------------------------------------------*/
/*!
 *  pixConvertTo32()
 *
 *      Input:  pixs (1, 2, 4, 8, 16 or 32 bpp)
 *      Return: pixd (32 bpp), or null on error
 *
 *  Usage: Top-level function, with simple default values for unpacking.
 *      1 bpp:  val0 = 255, val1 = 0
 *              and then replication into R, G and B components
 *      2 bpp:  if colormapped, use the colormap values; otherwise,
 *              use val0 = 0, val1 = 0x55, val2 = 0xaa, val3 = 255
 *              and replicate gray into R, G and B components
 *      4 bpp:  if colormapped, use the colormap values; otherwise,
 *              replicate 2 nybs into a byte, and then into R,G,B components
 *      8 bpp:  if colormapped, use the colormap values; otherwise,
 *              replicate gray values into R, G and B components
 *      16 bpp: replicate MSB into R, G and B components
 *      24 bpp: unpack the pixels, maintaining word alignment on each scanline
 *      32 bpp: makes a copy
 *
 *  Notes:
 *      (1) Implicit assumption about RGB component ordering.
 *}
function pixConvertTo32( pixs: PLPix): PLPix; cdecl; external LIBLEPT;

{*---------------------------------------------------------------------------*
 *                     Top-level conversion to 8 bpp                         *
 *---------------------------------------------------------------------------*/
/*!
 *  pixConvertTo8()
 *
 *      Input:  pixs (1, 2, 4, 8, 16 or 32 bpp)
 *              cmapflag (TRUE if pixd is to have a colormap; FALSE otherwise)
 *      Return: pixd (8 bpp), or null on error
 *
 *  Notes:
 *      (1) This is a top-level function, with simple default values
 *          for unpacking.
 *      (2) The result, pixd, is made with a colormap if specified.
 *      (3) If d == 8, and cmapflag matches the existence of a cmap
 *          in pixs, the operation is lossless and it returns a copy.
 *      (4) The default values used are:
 *          - 1 bpp: val0 = 255, val1 = 0
 *          - 2 bpp: 4 bpp:  even increments over dynamic range
 *          - 8 bpp: lossless if cmap matches cmapflag
 *          - 16 bpp: use most significant byte
 *      (5) If 32 bpp RGB, this is converted to gray.  If you want
 *          to do color quantization, you must specify the type
 *          explicitly, using the color quantization code.
 *}
function pixConvertTo8( pixs: PLPix; cmpflag: longint ): PLPix; cdecl; external LIBLEPT;

