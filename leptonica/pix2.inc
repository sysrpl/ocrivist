{ This file is an automated translation to pascal of pix2.c                   }
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
 *  pix2.c
 *
 *    This file has these basic operations:
 *
 *      (1) Get and set: individual pixels, full image, rectangular region,
 *          pad pixels, border pixels, and color components for RGB
 *      (2) Add and remove border pixels
 *      (3) Endian byte swaps
 *      (4) Simple method for byte-processing images (instead of words)
 *
 *      Pixel poking
 *           l_int32     pixGetPixel()
 *           l_int32     pixSetPixel()
 *           l_int32     pixGetRGBPixel()
 *           l_int32     pixSetRGBPixel()
 *           l_int32     pixGetRandomPixel()
 *           l_int32     pixClearPixel()
 *           l_int32     pixFlipPixel()
 *           void        setPixelLow()
 *
 *      Full image clear/set/set-to-arbitrary-value
 *           l_int32     pixClearAll()
 *           l_int32     pixSetAll()
 *           l_int32     pixSetAllArbitrary()
 *           l_int32     pixSetBlackOrWhite()
 *
 *      Rectangular region clear/set/set-to-arbitrary-value/blend
 *           l_int32     pixClearInRect()
 *           l_int32     pixSetInRect()
 *           l_int32     pixSetInRectArbitrary()
 *           l_int32     pixBlendInRect()
 *
 *      Set pad bits
 *           l_int32     pixSetPadBits()
 *           l_int32     pixSetPadBitsBand()
 *
 *      Assign border pixels
 *           l_int32     pixSetOrClearBorder()
 *           l_int32     pixSetBorderVal()
 *           l_int32     pixSetBorderRingVal()
 *           l_int32     pixSetMirroredBorder()
 *           PIX        *pixCopyBorder()
 *
 *      Add and remove border
 *           PIX        *pixAddBorder()
 *           PIX        *pixAddBlackBorder()
 *           PIX        *pixAddBorderGeneral()
 *           PIX        *pixRemoveBorder()
 *           PIX        *pixRemoveBorderGeneral()
 *           PIX        *pixAddMirroredBorder()
 *           PIX        *pixAddRepeatedBorder()
 *           PIX        *pixAddMixedBorder()
 *
 *      Color sample setting and extraction
 *           PIX        *pixCreateRGBImage()
 *           PIX        *pixGetRGBComponent()
 *           l_int32     pixSetRGBComponent()
 *           PIX        *pixGetRGBComponentCmap()
 *           l_int32     composeRGBPixel()
 *           void        extractRGBValues()
 *           l_int32     extractMinMaxComponent()
 *           l_int32     pixGetRGBLine()
 *
 *      Conversion between big and little endians
 *           PIX        *pixEndianByteSwapNew()
 *           l_int32     pixEndianByteSwap()
 *           l_int32     lineEndianByteSwap()
 *           PIX        *pixEndianTwoByteSwapNew()
 *           l_int32     pixEndianTwoByteSwap()
 *
 *      Extract raster data as binary string
 *           l_int32     pixGetRasterData()
 *
 *      Setup helpers for 8 bpp byte processing
 *           l_uint8   **pixSetupByteProcessing()
 *           l_int32     pixCleanupByteProcessing()
 *
 *      Setting parameters for antialias masking with alpha transforms
 *           void        l_setAlphaMaskBorder()
 *
 *      *** indicates implicit assumption about RGB component ordering
 *}

    {* This is a global that determines the default 8 bpp alpha mask values
     * for rings at distance 1 and 2 from the border.  Declare extern
     * to use.  To change the values, use l_setAlphaMaskBorder(). *
     LEPT_DLL l_float32  AlphaMaskBorderVals[2] = {0.0, 0.5};      }


{*-------------------------------------------------------------*
 *                         Pixel poking                        *
 *-------------------------------------------------------------*}

{*!
 *  pixGetPixel()
 *
 *      Input:  pix
 *              (x,y) pixel coords
 *              &val (<return> pixel value)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This returns the value in the data array.  If the pix is
 *          colormapped, it returns the colormap index, not the rgb value.
 *      (2) Because of the function overhead and the parameter checking,
 *          this is much slower than using the GET_DATA_*() macros directly.
 *          Speed on a 1 Mpixel RGB image, using a 3 GHz machine:
 *            * pixGet/pixSet: ~25 Mpix/sec
 *            * GET_DATA/SET_DATA: ~350 MPix/sec
 *          If speed is important and you're doing random access into
 *          the pix, use pixGetLinePtrs() and the array access macros.
 *}
function pixGetPixel( pix: PLPix; x, y: LongInt; pval: Pointer ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetPixel()
 *
 *      Input:  pix
 *              (x,y) pixel coords
 *              val (value to be inserted)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Warning: the input value is not checked for overflow with respect
 *          the the depth of @pix, and the sign bit (if any) is ignored.
 *          * For d == 1, @val > 0 sets the bit on.
 *          * For d == 2, 4, 8 and 16, @val is masked to the maximum allowable
 *            pixel value, and any (invalid) higher order bits are discarded.
 *      (2) See pixGetPixel() for information on performance.
 *}
function pixSetPixel( pix: PLPix; x, y: LongInt; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetRGBPixel()
 *
 *      Input:  pix (32 bpp rgb, not colormapped)
 *              (x,y) pixel coords
 *              &rval (<optional return> red component)
 *              &gval (<optional return> green component)
 *              &bval (<optional return> blue component)
 *      Return: 0 if OK; 1 on error
 *}
function pixGetRGBPixel( pix: PLPix; x, y: LongInt; prval, pgval, pbval: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetRGBPixel()
 *
 *      Input:  pix (32 bpp rgb)
 *              (x,y) pixel coords
 *              rval (red component)
 *              gval (green component)
 *              bval (blue component)
 *      Return: 0 if OK; 1 on error
 *}
function pixSetRGBPixel( pix: PLPix; x, y, rval, gval, bval: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetRandomPixel()
 *
 *      Input:  pix (any depth; can be colormapped)
 *              &val (<return> pixel value)
 *              &x (<optional return> x coordinate chosen; can be null)
 *              &y (<optional return> y coordinate chosen; can be null)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) If the pix is colormapped, it returns the rgb value.
 *}
function pixGetRandomPixel( pix: PLPix; pval: Pointer; px, py: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixClearPixel()
 *
 *      Input:  pix
 *              (x,y) pixel coords
 *      Return: 0 if OK; 1 on error.
 *}
function pixClearPixel( pix: PLPix; x, y: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixFlipPixel()
 *
 *      Input:  pix
 *              (x,y) pixel coords
 *      Return: 0 if OK; 1 on error
 *}
function pixFlipPixel( pix: PLPix; x, y: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  setPixelLow()
 *
 *      Input:  line (ptr to beginning of line),
 *              x (pixel location in line)
 *              depth (bpp)
 *              val (to be inserted)
 *      Return: void
 *
 *  Notes:
 *      (1) Caution: input variables are not checked!
 *}
procedure setPixelLow( line: Pointer; x, depth: LongInt; val: LongWord ) ; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *     Full image clear/set/set-to-arbitrary-value/invert      *
 *-------------------------------------------------------------*}

{*!
 *  pixClearAll()
 *
 *      Input:  pix (all depths; use cmapped with caution)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Clears all data to 0.  For 1 bpp, this is white; for grayscale
 *          or color, this is black.
 *      (2) Caution: for colormapped pix, this sets the color to the first
 *          one in the colormap.  Be sure that this is the intended color!
 *}
function pixClearAll( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetAll()
 *
 *      Input:  pix (all depths; use cmapped with caution)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Sets all data to 1.  For 1 bpp, this is black; for grayscale
 *          or color, this is white.
 *      (2) Caution: for colormapped pix, this sets the pixel value to the
 *          maximum value supported by the colormap: 2^d - 1.  However, this
 *          color may not be defined, because the colormap may not be full.
 *}
function pixSetAll( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetAllArbitrary()
 *
 *      Input:  pix (all depths; use cmapped with caution)
 *              val  (value to set all pixels)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) For colormapped pix, be sure the value is the intended
 *          one in the colormap.
 *      (2) Caution: for colormapped pix, this sets each pixel to the
 *          color at the index equal to val.  Be sure that this index
 *          exists in the colormap and that it is the intended one!
 *}
function pixSetAllArbitrary( pix: PLPix; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetBlackOrWhite()
 *
 *      Input:  pixs (all depths; cmap ok)
 *              op (L_SET_BLACK, L_SET_WHITE)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Function for setting all pixels in an image to either black
 *          or white.
 *      (2) If pixs is colormapped, it adds black or white to the
 *          colormap if it's not there and there is room.  If the colormap
 *          is full, it finds the closest color in intensity.
 *          This index is written to all pixels.
 *}
function pixSetBlackOrWhite( pixs: PLPix; op: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *     Rectangular region clear/set/set-to-arbitrary-value     *
 *-------------------------------------------------------------*}

{*!
 *  pixClearInRect()
 *
 *      Input:  pix (all depths; can be cmapped)
 *              box (in which all pixels will be cleared)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Clears all data in rect to 0.  For 1 bpp, this is white;
 *          for grayscale or color, this is black.
 *      (2) Caution: for colormapped pix, this sets the color to the first
 *          one in the colormap.  Be sure that this is the intended color!
 *}
function pixClearInRect( pix: PLPix; box: PLBox ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetInRect()
 *
 *      Input:  pix (all depths, can be cmapped)
 *              box (in which all pixels will be set)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Sets all data in rect to 1.  For 1 bpp, this is black;
 *          for grayscale or color, this is white.
 *      (2) Caution: for colormapped pix, this sets the pixel value to the
 *          maximum value supported by the colormap: 2^d - 1.  However, this
 *          color may not be defined, because the colormap may not be full.
 *}
function pixSetInRect( pix: PLPix; box: PLBox ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetInRectArbitrary()
 *
 *      Input:  pix (all depths; can be cmapped)
 *              box (in which all pixels will be set to val)
 *              val  (value to set all pixels)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) For colormapped pix, be sure the value is the intended
 *          one in the colormap.
 *      (2) Caution: for colormapped pix, this sets each pixel in the
 *          rect to the color at the index equal to val.  Be sure that
 *          this index exists in the colormap and that it is the intended one!
 *}
function pixSetInRectArbitrary( pix: PLPix; box: PLBox; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixBlendInRect()
 *
 *      Input:  pixs (32 bpp rgb)
 *              box (<optional> in which all pixels will be blended)
 *              val  (blend value; 0xrrggbb00)
 *              fract (fraction of color to be blended with each pixel in pixs)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This is an in-place function.  It blends the input color @val
 *          with the pixels in pixs in the specified rectangle.
 *          If no rectangle is specified, it blends over the entire image.
 *}
function pixBlendInRect( pixs: PLPix; box: PLBox; val: LongWord; fract: Single ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                         Set pad bits                        *
 *-------------------------------------------------------------*}

{*!
 *  pixSetPadBits()
 *
 *      Input:  pix (1, 2, 4, 8, 16, 32 bpp)
 *              val  (0 or 1)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The pad bits are the bits that expand each scanline to a
 *          multiple of 32 bits.  They are usually not used in
 *          image processing operations.  When boundary conditions
 *          are important, as in seedfill, they must be set properly.
 *      (2) This sets the value of the pad bits (if any) in the last
 *          32-bit word in each scanline.
 *      (3) For 32 bpp pix, there are no pad bits, so this is a no-op.
 *}
function pixSetPadBits( pix: PLPix; val: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetPadBitsBand()
 *
 *      Input:  pix (1, 2, 4, 8, 16, 32 bpp)
 *              by  (starting y value of band)
 *              bh  (height of band)
 *              val  (0 or 1)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The pad bits are the bits that expand each scanline to a
 *          multiple of 32 bits.  They are usually not used in
 *          image processing operations.  When boundary conditions
 *          are important, as in seedfill, they must be set properly.
 *      (2) This sets the value of the pad bits (if any) in the last
 *          32-bit word in each scanline, within the specified
 *          band of raster lines.
 *      (3) For 32 bpp pix, there are no pad bits, so this is a no-op.
 *}
function pixSetPadBitsBand( pix: PLPix; by, bh, val: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                       Set border pixels                     *
 *-------------------------------------------------------------*}

{*!
 *  pixSetOrClearBorder()
 *
 *      Input:  pixs (all depths)
 *              left, right, top, bot (amount to set or clear)
 *              operation (PIX_SET or PIX_CLR)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The border region is defined to be the region in the
 *          image within a specific distance of each edge.  Here, we
 *          allow the pixels within a specified distance of each
 *          edge to be set independently.  This either sets or
 *          clears all pixels in the border region.
 *      (2) For binary images, use PIX_SET for black and PIX_CLR for white.
 *      (3) For grayscale or color images, use PIX_SET for white
 *          and PIX_CLR for black.
 *}
function pixSetOrClearBorder( pixs: PLPix; left, right, top, bot, op: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetBorderVal()
 *
 *      Input:  pixs (8, 16 or 32 bpp)
 *              left, right, top, bot (amount to set)
 *              val (value to set at each border pixel)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The border region is defined to be the region in the
 *          image within a specific distance of each edge.  Here, we
 *          allow the pixels within a specified distance of each
 *          edge to be set independently.  This sets the pixels
 *          in the border region to the given input value.
 *      (2) For efficiency, use pixSetOrClearBorder() if
 *          you're setting the border to either black or white.
 *      (3) If d != 32, the input value should be masked off
 *          to the appropriate number of least significant bits.
 *      (4) The code is easily generalized for 2 or 4 bpp.
 *}
function pixSetBorderVal( pixs: PLPix; left, right, top, bot: LongInt; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetBorderRingVal()
 *
 *      Input:  pixs (any depth; cmap OK)
 *              dist (distance from outside; must be > 0; first ring is 1)
 *              val (value to set at each border pixel)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The rings are single-pixel-wide rectangular sets of
 *          pixels at a given distance from the edge of the pix.
 *          This sets all pixels in a given ring to a value.
 *}
function pixSetBorderRingVal( pixs: PLPix; dist: LongInt; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetMirroredBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot (number of pixels to set)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This applies what is effectively mirror boundary conditions
 *          to a border region in the image.  It is in-place.
 *      (2) This is useful for setting pixels near the border to a
 *          value representative of the near pixels to the interior.
 *      (3) The general pixRasterop() is used for an in-place operation here
 *          because there is no overlap between the src and dest rectangles.
 *}
function pixSetMirroredBorder( pixs: PLPix; left, right, top, bot: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCopyBorder()
 *
 *      Input:  pixd (all depths; colormap ok; can be NULL)
 *              pixs (same depth and size as pixd)
 *              left, right, top, bot (number of pixels to copy)
 *      Return: pixd, or null on error if pixd is not defined
 *
 *  Notes:
 *      (1) pixd can be null, but otherwise it must be the same size
 *          and depth as pixs.  Always returns pixd.
 *      (1) This is useful in situations where by setting a few border
 *          pixels we can avoid having to copy all pixels in pixs into
 *          pixd as an initialization step for some operation.
 *}
function pixCopyBorder( pixd, pixs: PLPix; left, right, top, bot: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                     Add and remove border                   *
 *-------------------------------------------------------------*}

{*!
 *  pixAddBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              npix (number of pixels to be added to each side)
 *              val  (value of added border pixels)
 *      Return: pixd (with the added exterior pixels), or null on error
 *
 *  Notes:
 *      (1) See pixAddBorderGeneral() for values of white & black pixels.
 *}
function pixAddBorder( pixs: PLPix; npix: LongInt; val: LongWord ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAddBlackBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              npix (number of pixels to be added to each side)
 *      Return: pixd (with the added exterior pixels), or null on error
 *}
function pixAddBlackBorder( pixs: PLPix; npix: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAddBorderGeneral()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot  (number of pixels added)
 *              val   (value of added border pixels)
 *      Return: pixd (with the added exterior pixels), or null on error
 *
 *  Notes:
 *      (1) For binary images:
 *             white:  val = 0
 *             black:  val = 1
 *          For grayscale images:
 *             white:  val = 2 ** d - 1
 *             black:  val = 0
 *          For rgb color images:
 *             white:  val = 0xffffff00
 *             black:  val = 0
 *          For colormapped images, use 'index' found this way:
 *             white: pixcmapGetRankIntensity(cmap, 1.0, &index);
 *             black: pixcmapGetRankIntensity(cmap, 0.0, &index);
 *}
function pixAddBorderGeneral( pixs: PLPix; left, right, top, bot: LongInt; val: LongWord ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRemoveBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              npix (number to be removed from each of the 4 sides)
 *      Return: pixd (with pixels removed around border), or null on error
 *}
function pixRemoveBorder( pixs: PLPix; npix: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRemoveBorderGeneral()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot  (number of pixels added)
 *      Return: pixd (with pixels removed around border), or null on error
 *}
function pixRemoveBorderGeneral( pixs: PLPix; left, right, top, bot: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAddMirroredBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot (number of pixels added)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This applies what is effectively mirror boundary conditions.
 *          For the added border pixels in pixd, the pixels in pixs
 *          near the border are mirror-copied into the border region.
 *      (2) This is useful for avoiding special operations near
 *          boundaries when doing image processing operations
 *          such as rank filters and convolution.  In use, one first
 *          adds mirrored pixels to each side of the image.  The number
 *          of pixels added on each side is half the filter dimension.
 *          Then the image processing operations proceed over a
 *          region equal to the size of the original image, and
 *          write directly into a dest pix of the same size as pixs.
 *      (3) The general pixRasterop() is used for an in-place operation here
 *          because there is no overlap between the src and dest rectangles.
 *}
function pixAddMirroredBorder( pixs: PLPix; left, right, top, bot: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAddRepeatedBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot (number of pixels added)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This applies a repeated border, as if the central part of
 *          the image is tiled over the plane.  So, for example, the
 *          pixels in the left border come from the right side of the image.
 *      (2) The general pixRasterop() is used for an in-place operation here
 *          because there is no overlap between the src and dest rectangles.
 *}
function pixAddRepeatedBorder( pixs: PLPix; left, right, top, bot: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAddMixedBorder()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              left, right, top, bot (number of pixels added)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This applies mirrored boundary conditions horizontally
 *          and repeated b.c. vertically.
 *      (2) It is specifically used for avoiding special operations
 *          near boundaries when convolving a hue-saturation histogram
 *          with a given window size.  The repeated b.c. are used
 *          vertically for hue, and the mirrored b.c. are used
 *          horizontally for saturation.  The number of pixels added
 *          on each side is approximately (but not quite) half the
 *          filter dimension.  The image processing operations can
 *          then proceed over a region equal to the size of the original
 *          image, and write directly into a dest pix of the same
 *          size as pixs.
 *      (3) The general pixRasterop() can be used for an in-place
 *          operation here because there is no overlap between the
 *          src and dest rectangles.
 *}
function pixAddMixedBorder( pixs: PLPix; left, right, top, bot: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                Color sample setting and extraction          *
 *-------------------------------------------------------------*}

{*!
 *  pixCreateRGBImage()
 *
 *      Input:  8 bpp red pix
 *              8 bpp green pix
 *              8 bpp blue pix
 *      Return: 32 bpp pix, interleaved with 4 samples/pixel,
 *              or null on error
 *
 *  Notes:
 *      (1) the 4th byte, sometimes called the "alpha channel",
 *          and which is often used for blending between different
 *          images, is left with 0 value.
 *      (2) see Note (4) in pix.h for details on storage of
 *          8-bit samples within each 32-bit word.
 *      (3) This implementation, setting the r, g and b components
 *          sequentially, is much faster than setting them in parallel
 *          by constructing an RGB dest pixel and writing it to dest.
 *          The reason is there are many more cache misses when reading
 *          from 3 input images simultaneously.
 *}
function pixCreateRGBImage( pixr, pixg, pixb: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixGetRGBComponent()
 *
 *      Input:  pixs  (32 bpp)
 *              color  (one of {COLOR_RED, COLOR_GREEN, COLOR_BLUE,
 *                      L_ALPHA_CHANNEL})
 *      Return: pixd, the selected 8 bpp component image of the
 *              input 32 bpp image, or null on error
 *
 *  Notes:
 *      (1) The alpha channel (in the 4th byte of each RGB pixel)
 *          is mostly ignored in leptonica.
 *      (2) Three calls to this function generate the three 8 bpp component
 *          images.  This is much faster than generating the three
 *          images in parallel, by extracting a src pixel and setting
 *          the pixels of each component image from it.  The reason is
 *          there are many more cache misses when writing to three
 *          output images simultaneously.
 *}
function pixGetRGBComponent( pixs: PLPix; color: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixSetRGBComponent()
 *
 *      Input:  pixd  (32 bpp)
 *              pixs  (8 bpp)
 *              color  (one of {COLOR_RED, COLOR_GREEN, COLOR_BLUE,
 *                      L_ALPHA_CHANNEL})
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This places the 8 bpp pixel in pixs into the
 *          specified color component (properly interleaved) in pixd.
 *      (2) The alpha channel component mostly ignored in leptonica.
 *}
function pixSetRGBComponent( pixd, pixs: PLPix; color: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetRGBComponentCmap()
 *
 *      Input:  pixs  (colormapped)
 *              color  (one of {COLOR_RED, COLOR_GREEN, COLOR_BLUE})
 *      Return: pixd  (the selected 8 bpp component image of the
 *                     input cmapped image), or null on error
 *}
function pixGetRGBComponentCmap( pixs: PLPix; color: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  composeRGBPixel()
 *
 *      Input:  rval, gval, bval
 *              &rgbpixel  (<return> 32-bit pixel)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) A slower implementation uses macros:
 *            SET_DATA_BYTE(ppixel, COLOR_RED, rval);
 *            SET_DATA_BYTE(ppixel, COLOR_GREEN, gval);
 *            SET_DATA_BYTE(ppixel, COLOR_BLUE, bval);
 *}
function composeRGBPixel( rval, gval, bval: LongInt; ppixel: Pointer ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  extractRGBValues()
 *
 *      Input:  pixel (32 bit)
 *              &rval (<optional return> red component)
 *              &gval (<optional return> green component)
 *              &bval (<optional return> blue component)
 *      Return: void
 *
 *  Notes:
 *      (1) A slower implementation uses macros:
 *             *prval = GET_DATA_BYTE(&pixel, COLOR_RED);
 *             *pgval = GET_DATA_BYTE(&pixel, COLOR_GREEN);
 *             *pbval = GET_DATA_BYTE(&pixel, COLOR_BLUE);
 *}
procedure extractRGBValues( pixel: LongWord; prval, pgval, pbval: PLongint ) ; cdecl; external LIBLEPT;


{*!
 *  extractMinMaxComponent()
 *
 *      Input:  pixel (32 bpp RGB)
 *              type (L_CHOOSE_MIN or L_CHOOSE_MAX)
 *      Return: componet (in range [0 ... 255], or null on error
 *}
function extractMinMaxComponent( pixel: LongWord; L_type: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetRGBLine()
 *
 *      Input:  pixs  (32 bpp)
 *              row
 *              bufr  (array of red samples; size w bytes)
 *              bufg  (array of green samples; size w bytes)
 *              bufb  (array of blue samples; size w bytes)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This puts rgb components from the input line in pixs
 *          into the given buffers.
 *}
function pixGetRGBLine( pixs: PLPix; row: LongInt; bufr, bufg, bufb: PByte ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                    Pixel endian conversion                  *
 *-------------------------------------------------------------*}

{*!
 *  pixEndianByteSwapNew()
 *
 *      Input:  pixs
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This is used to convert the data in a pix to a
 *          serialized byte buffer in raster order, and, for RGB,
 *          in order RGBA.  This requires flipping bytes within
 *          each 32-bit word for little-endian platforms, because the
 *          words have a MSB-to-the-left rule, whereas byte raster-order
 *          requires the left-most byte in each word to be byte 0.
 *          For big-endians, no swap is necessary, so this returns a clone.
 *      (2) Unlike pixEndianByteSwap(), which swaps the bytes in-place,
 *          this returns a new pix (or a clone).  We provide this
 *          because often when serialization is done, the source
 *          pix needs to be restored to canonical little-endian order,
 *          and this requires a second byte swap.  In such a situation,
 *          it is twice as fast to make a new pix in big-endian order,
 *          use it, and destroy it.
 *}
function pixEndianByteSwapNew( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixEndianByteSwap()
 *
 *      Input:  pixs
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is used on little-endian platforms to swap
 *          the bytes within a word; bytes 0 and 3 are swapped,
 *          and bytes 1 and 2 are swapped.
 *      (2) This is required for little-endians in situations
 *          where we convert from a serialized byte order that is
 *          in raster order, as one typically has in file formats,
 *          to one with MSB-to-the-left in each 32-bit word, or v.v.
 *          See pix.h for a description of the canonical format
 *          (MSB-to-the left) that is used for both little-endian
 *          and big-endian platforms.   For big-endians, the
 *          MSB-to-the-left word order has the bytes in raster
 *          order when serialized, so no byte flipping is required.
 *}
function pixEndianByteSwap( pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  lineEndianByteSwap()
 *
 *      Input   datad (dest byte array data, reordered on little-endians)
 *              datas (a src line of pix data)
 *              wpl (number of 32 bit words in the line)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is used on little-endian platforms to swap
 *          the bytes within each word in the line of image data.
 *          Bytes 0 <==> 3 and 1 <==> 2 are swapped in the dest
 *          byte array data8d, relative to the pix data in datas.
 *      (2) The bytes represent 8 bit pixel values.  They are swapped
 *          for little endians so that when the dest array (char *)datad
 *          is addressed by bytes, the pixels are chosen sequentially
 *          from left to right in the image.
 *}
function lineEndianByteSwap( datad, datas: Pointer; wpl: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixEndianTwoByteSwapNew()
 *
 *      Input:  pixs
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is used on little-endian platforms to swap the
 *          2-byte entities within a 32-bit word.
 *      (2) This is equivalent to a full byte swap, as performed
 *          by pixEndianByteSwap(), followed by byte swaps in
 *          each of the 16-bit entities separately.
 *      (3) Unlike pixEndianTwoByteSwap(), which swaps the shorts in-place,
 *          this returns a new pix (or a clone).  We provide this
 *          to avoid having to swap twice in situations where the input
 *          pix must be restored to canonical little-endian order.
 *}
function pixEndianTwoByteSwapNew( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixEndianTwoByteSwap()
 *
 *      Input:  pixs
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is used on little-endian platforms to swap the
 *          2-byte entities within a 32-bit word.
 *      (2) This is equivalent to a full byte swap, as performed
 *          by pixEndianByteSwap(), followed by byte swaps in
 *          each of the 16-bit entities separately.
 *}
function pixEndianTwoByteSwap( pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *             Extract raster data as binary string            *
 *-------------------------------------------------------------*}

{*!
 *  pixGetRasterData()
 *
 *      Input:  pixs (1, 8, 32 bpp)
 *              &data (<return> raster data in memory)
 *              &nbytes (<return> number of bytes in data string)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This returns the raster data as a byte string, padded to the
 *          byte.  For 1 bpp, the first pixel is the MSbit in the first byte.
 *          For rgb, the bytes are in (rgb) order.  This is the format
 *          required for flate encoding of pixels in a PostScript file.
 *}
function pixGetRasterData( pixs: PLPix; pdata: PPByte; pnbytes: PLongWord ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *             Setup helpers for 8 bpp byte processing         *
 *-------------------------------------------------------------*}

{*!
 *  pixSetupByteProcessing()
 *
 *      Input:  pix (8 bpp, no colormap)
 *              &w (<optional return> width)
 *              &h (<optional return> height)
 *      Return: line ptr array, or null on error
 *
 *  Notes:
 *      (1) This is a simple helper for processing 8 bpp images with
 *          direct byte access.  It can swap byte order within each word.
 *      (2) After processing, you must call pixCleanupByteProcessing(),
 *          which frees the lineptr array and restores byte order.
 *      (3) Usage:
 *              l_uint8 **lineptrs = pixSetupByteProcessing(pix, &w, &h);
 *              for (i = 0; i < h; i++) {
 *                  l_uint8 *line = lineptrs[i];
 *                  for (j = 0; j < w; j++) {
 *                      val = line[j];
 *                      ...
 *                  }
 *              }
 *              pixCleanupByteProcessing(pix, lineptrs);
 *}
function pixSetupByteProcessing( pix: PLPix; pw, ph: PLongint ) : PPByte; cdecl; external LIBLEPT;


{*!
 *  pixCleanupByteProcessing()
 *
 *      Input:  pix (8 bpp, no colormap)
 *              lineptrs (ptrs to the beginning of each raster line of data)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This must be called after processing that was initiated
 *          by pixSetupByteProcessing() has finished.
 *}
function pixCleanupByteProcessing( pix: PLPix; lineptrs: PPByte ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------------*
 *      Setting parameters for antialias masking with alpha transforms    *
 *------------------------------------------------------------------------*}

{*!
 *  l_setAlphaMaskBorder()
 *
 *      Input:  val1, val2 (in [0.0 ... 1.0])
 *      Return: void
 *
 *  Notes:
 *      (1) This sets the opacity values used to generate the two outer
 *          boundary rings in the alpha mask associated with geometric
 *          transforms such as pixRotateWithAlpha().
 *      (2) The default values are val1 = 0.0 (completely transparent
 *          in the outermost ring) and val2 = 0.5 (half transparent
 *          in the second ring).  When the image is blended, this
 *          completely removes the outer ring (shrinking the image by
 *          2 in each direction), and alpha-blends with 0.5 the second ring.
 *      (3) The actual mask values are found by multiplying these
 *          normalized opacity values by 255.
 *}
procedure l_setAlphaMaskBorder( val1, val2: Single ) ; cdecl; external LIBLEPT;


