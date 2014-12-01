{ This file is an automated translation to pascal of pix3.c                   }
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
 *  pix3.c
 *
 *    This file has these operations:
 *
 *      (1) Mask-directed operations
 *      (2) Full-image bit-logical operations
 *      (3) Foreground pixel counting operations on 1 bpp images
 *      (4) Sum of pixel values
 *      (5) Mirrored tiling of a smaller image
 *
 *
 *    Masked operations
 *           l_int32     pixSetMasked()
 *           l_int32     pixSetMaskedGeneral()
 *           l_int32     pixCombineMasked()
 *           l_int32     pixCombineMaskedGeneral()
 *           l_int32     pixPaintThroughMask()
 *           PIX        *pixPaintSelfThroughMask()
 *           PIX        *pixMakeMaskFromLUT()
 *           PIX        *pixSetUnderTransparency()
 *
 *    One and two-image boolean operations on arbitrary depth images
 *           PIX        *pixInvert()
 *           PIX        *pixOr()
 *           PIX        *pixAnd()
 *           PIX        *pixXor()
 *           PIX        *pixSubtract()
 *
 *    Foreground pixel counting in 1 bpp images
 *           l_int32     pixZero()
 *           l_int32     pixForegroundFraction()
 *           l_int32     pixCountPixels()
 *           NUMA       *pixaCountPixels()
 *           l_int32     pixCountPixelsInRow()
 *           NUMA       *pixCountPixelsByRow()
 *           NUMA       *pixCountPixelsByColumn()
 *           NUMA       *pixSumPixelsByRow()
 *           NUMA       *pixSumPixelsByColumn()
 *           l_int32     pixThresholdPixelSum()
 *           l_int32    *makePixelSumTab8()
 *           l_int32    *makePixelCentroidTab8()
 *
 *    Pixel counting in gray and colormapped images
 *           l_int32     pixCountArbInRect()
 *
 *    Sum of pixel values
 *           l_int32     pixSumPixelValues()
 *
 *    Mirrored tiling
 *           PIX        *pixMirroredTiling()
 *
 *    Static helper function
 *           static l_int32  findTilePatchCenter()
 *}

{* ~NO_CONSOLE_IO *}

{*-------------------------------------------------------------*
 *                        Masked operations                    *
 *-------------------------------------------------------------*}

{*!
 *  pixSetMasked()
 *
 *      Input:  pixd (1, 2, 4, 8, 16 or 32 bpp; or colormapped)
 *              pixm (<optional> 1 bpp mask; no operation if NULL)
 *              val (value to set at each masked pixel)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) In-place operation.
 *      (2) NOTE: For cmapped images, this calls pixSetMaskedCmap().
 *          @val must be the 32-bit color representation of the RGB pixel.
 *          It is not the index into the colormap!
 *      (2) If pixm == NULL, a warning is given.
 *      (3) This is an implicitly aligned operation, where the UL
 *          corners of pixd and pixm coincide.  A warning is
 *          issued if the two image sizes differ significantly,
 *          but the operation proceeds.
 *      (4) Each pixel in pixd that co-locates with an ON pixel
 *          in pixm is set to the specified input value.
 *          Other pixels in pixd are not changed.
 *      (5) You can visualize this as painting the color through
 *          the mask, as a stencil.
 *      (6) If you do not want to have the UL corners aligned,
 *          use the function pixSetMaskedGeneral(), which requires
 *          you to input the UL corner of pixm relative to pixd.
 *      (7) Implementation details: see comments in pixPaintThroughMask()
 *          for when we use rasterop to do the painting.
 *}
function pixSetMasked( pixd, pixm: PLPix; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetMaskedGeneral()
 *
 *      Input:  pixd (8, 16 or 32 bpp)
 *              pixm (<optional> 1 bpp mask; no operation if null)
 *              val (value to set at each masked pixel)
 *              x, y (location of UL corner of pixm relative to pixd;
 *                    can be negative)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This is an in-place operation.
 *      (2) Alignment is explicit.  If you want the UL corners of
 *          the two images to be aligned, use pixSetMasked().
 *      (3) A typical use would be painting through the foreground
 *          of a small binary mask pixm, located somewhere on a
 *          larger pixd.  Other pixels in pixd are not changed.
 *      (4) You can visualize this as painting the color through
 *          the mask, as a stencil.
 *      (5) This uses rasterop to handle clipping and different depths of pixd.
 *      (6) If pixd has a colormap, you should call pixPaintThroughMask().
 *      (7) Why is this function here, if pixPaintThroughMask() does the
 *          same thing, and does it more generally?  I've retained it here
 *          to show how one can paint through a mask using only full
 *          image rasterops, rather than pixel peeking in pixm and poking
 *          in pixd.  It's somewhat baroque, but I found it amusing.
 *}
function pixSetMaskedGeneral( pixd, pixm: PLPix; val: LongWord; x, y: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCombineMasked()
 *
 *      Input:  pixd (1 bpp, 8 bpp gray or 32 bpp rgb; no cmap)
 *              pixs (1 bpp, 8 bpp gray or 32 bpp rgb; no cmap)
 *              pixm (<optional> 1 bpp mask; no operation if NULL)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) In-place operation; pixd is changed.
 *      (2) This sets each pixel in pixd that co-locates with an ON
 *          pixel in pixm to the corresponding value of pixs.
 *      (3) pixs and pixd must be the same depth and not colormapped.
 *      (4) All three input pix are aligned at the UL corner, and the
 *          operation is clipped to the intersection of all three images.
 *      (5) If pixm == NULL, it's a no-op.
 *      (6) Implementation: see notes in pixCombineMaskedGeneral().
 *          For 8 bpp selective masking, you might guess that it
 *          would be faster to generate an 8 bpp version of pixm,
 *          using pixConvert1To8(pixm, 0, 255), and then use a
 *          general combine operation
 *               d = (d & ~m) | (s & m)
 *          on a word-by-word basis.  Not always.  The word-by-word
 *          combine takes a time that is independent of the mask data.
 *          If the mask is relatively sparse, the byte-check method
 *          is actually faster!
 *}
function pixCombineMasked( pixd, pixs, pixm: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCombineMaskedGeneral()
 *
 *      Input:  pixd (1 bpp, 8 bpp gray or 32 bpp rgb)
 *              pixs (1 bpp, 8 bpp gray or 32 bpp rgb)
 *              pixm (<optional> 1 bpp mask)
 *              x, y (origin of pixs and pixm relative to pixd; can be negative)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) In-place operation; pixd is changed.
 *      (2) This is a generalized version of pixCombinedMasked(), where
 *          the source and mask can be placed at the same (arbitrary)
 *          location relative to pixd.
 *      (3) pixs and pixd must be the same depth and not colormapped.
 *      (4) The UL corners of both pixs and pixm are aligned with
 *          the point (x, y) of pixd, and the operation is clipped to
 *          the intersection of all three images.
 *      (5) If pixm == NULL, it's a no-op.
 *      (6) Implementation.  There are two ways to do these.  In the first,
 *          we use rasterop, ORing the part of pixs under the mask
 *          with pixd (which has been appropriately cleared there first).
 *          In the second, the mask is used one pixel at a time to
 *          selectively replace pixels of pixd with those of pixs.
 *          Here, we use rasterop for 1 bpp and pixel-wise replacement
 *          for 8 and 32 bpp.  To use rasterop for 8 bpp, for example,
 *          we must first generate an 8 bpp version of the mask.
 *          The code is simple:
 *
 *             Pix *pixm8 = pixConvert1To8(NULL, pixm, 0, 255);
 *             Pix *pixt = pixAnd(NULL, pixs, pixm8);
 *             pixRasterop(pixd, x, y, wmin, hmin, PIX_DST & PIX_NOT(PIX_SRC),
 *                         pixm8, 0, 0);
 *             pixRasterop(pixd, x, y, wmin, hmin, PIX_SRC | PIX_DST,
 *                         pixt, 0, 0);
 *             pixDestroy(&pixt);
 *             pixDestroy(&pixm8);
 *}
function pixCombineMaskedGeneral( pixd, pixs, pixm: PLPix; x, y: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixPaintThroughMask()
 *
 *      Input:  pixd (1, 2, 4, 8, 16 or 32 bpp; or colormapped)
 *              pixm (<optional> 1 bpp mask)
 *              x, y (origin of pixm relative to pixd; can be negative)
 *              val (pixel value to set at each masked pixel)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) In-place operation.  Calls pixSetMaskedCmap() for colormapped
 *          images.
 *      (2) For 1, 2, 4, 8 and 16 bpp gray, we take the appropriate
 *          number of least significant bits of val.
 *      (3) If pixm == NULL, it's a no-op.
 *      (4) The mask origin is placed at (x,y) on pixd, and the
 *          operation is clipped to the intersection of rectangles.
 *      (5) For rgb, the components in val are in the canonical locations,
 *          with red in location COLOR_RED, etc.
 *      (6) Implementation detail 1:
 *          For painting with val == 0 or val == maxval, you can use rasterop.
 *          If val == 0, invert the mask so that it's 0 over the region
 *          into which you want to write, and use PIX_SRC & PIX_DST to
 *          clear those pixels.  To write with val = maxval (all 1's),
 *          use PIX_SRC | PIX_DST to set all bits under the mask.
 *      (7) Implementation detail 2:
 *          The rasterop trick can be used for depth > 1 as well.
 *          For val == 0, generate the mask for depth d from the binary
 *          mask using
 *              pixmd = pixUnpackBinary(pixm, d, 1);
 *          and use pixRasterop() with PIX_MASK.  For val == maxval,
 *              pixmd = pixUnpackBinary(pixm, d, 0);
 *          and use pixRasterop() with PIX_PAINT.
 *          But note that if d == 32 bpp, it is about 3x faster to use
 *          the general implementation (not pixRasterop()).
 *      (8) Implementation detail 3:
 *          It might be expected that the switch in the inner loop will
 *          cause large branching delays and should be avoided.
 *          This is not the case, because the entrance is always the
 *          same and the compiler can correctly predict the jump.
 *}
function pixPaintThroughMask( pixd, pixm: PLPix; x, y: LongInt; val: LongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixPaintSelfThroughMask()
 *
 *      Input:  pixd (8 bpp gray or 32 bpp rgb; not colormapped)
 *              pixm (1 bpp mask)
 *              x, y (origin of pixm relative to pixd; must not be negative)
 *              tilesize (requested size for tiling)
 *              searchdir (L_HORIZ, L_VERT)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) In-place operation; pixd is changed.
 *      (2) If pixm == NULL, it's a no-op.
 *      (3) The mask origin is placed at (x,y) on pixd, and the
 *          operation is clipped to the intersection of pixd and the
 *          fg of the mask.
 *      (4) The tilesize is the the requested size for tiling.  The
 *          actual size for each c.c. will be bounded by the minimum
 *          dimension of the c.c. and the distance at which the tile
 *          center is located.
 *      (5) searchdir is the direction with respect to the b.b. of each
 *          mask component, from which the square patch is chosen and
 *          tiled onto the image, clipped by the mask component.
 *      (6) Specifically, a mirrored tiling, generated from pixd,
 *          is used to construct the pixels that are painted onto
 *          pixd through pixm.
 *}
function pixPaintSelfThroughMask( pixd, pixm: PLPix; x, y, tilesize, searchdir: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixMakeMaskFromLUT()
 *
 *      Input:  pixs (2, 4 or 8 bpp; can be colormapped)
 *              tab (256-entry LUT; 1 means to write to mask)
 *      Return: pixd (1 bpp mask), or null on error
 *
 *  Notes:
 *      (1) This generates a 1 bpp mask image, where a 1 is written in
 *          the mask for each pixel in pixs that has a value corresponding
 *          to a 1 in the LUT.
 *      (2) The LUT should be of size 256.
 *}
function pixMakeMaskFromLUT( pixs: PLPix; tab: PLongint ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixSetUnderTransparency()
 *
 *      Input:  pixs (32 bpp rgba)
 *              val (32 bit unsigned color to use where alpha == 0)
 *              debugflag (generates intermediate images)
 *      Return: pixd (32 bpp rgba), or null on error
 *
 *  Notes:
 *      (1) This is one of the few operations in leptonica that uses
 *          the alpha blending component in rgba images.  It sets
 *          the r, g and b components under every fully transparent alpha
 *          component to @val.
 *      (2) Full transparency is denoted by alpha == 0.  By setting
 *          all pixels to @val where alpha == 0, this can improve
 *          compressibility by reducing the entropy.
 *      (3) The visual result depends on how the image is displayed.
 *          (a) For display devices that respect the use of the alpha
 *              layer, this will not affect the appearance.
 *          (b) For typical leptonica operations, alpha is ignored,
 *              so there will be a change in appearance because this
 *              resets the rgb values in the fully transparent region.
 *      (4) For reading and writing rgba pix in png format, use
 *          pixReadRGBAPng() and pixWriteRGBAPng().
 *      (5) For example, if you want to rewrite all fully transparent
 *          pixels in a png file to white:
 *              pixs = pixReadRGBAPng(<infile>);  // special read
 *              pixd = pixSetUnderTransparency(pixs, 0xffffff00, 0);
 *          Then either use a normal write if you won't be using transparency:
 *              pixWrite(<outfile>, pixd, IFF_PNG);
 *          or an RGBA write if you want to preserve the transparency layer
 *              pixWriteRGBAPng(<outfile>, pixd);  // special write
 *      (6) Caution.  Because rgb images in leptonica typically
 *          have value 0 in the alpha channel, this function would
 *          interpret the entire image as fully transparent, and set
 *          every pixel to @val.  Because this is not desirable, instead
 *          we issue a warning and return a copy of the input pix.
 *          If you really want to set every pixel to the same value,
 *          use pixSetAllArbitrary().
 *      (7) This is useful for compressing an RGBA image where the part
 *          of the image that is fully transparent is random; compression
 *          is typically improved by setting that region to a constant.
 *          For rendering as a 3 component RGB image over a uniform
 *          background, use pixAlphaBlendUniform().
 *}
function pixSetUnderTransparency( pixs: PLPix; val: LongWord; debugflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *    One and two-image boolean ops on arbitrary depth images  *
 *-------------------------------------------------------------*}

{*!
 *  pixInvert()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs,
 *                     or different from pixs)
 *              pixs
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This inverts pixs, for all pixel depths.
 *      (2) There are 3 cases:
 *           (a) pixd == null,   ~src --> new pixd
 *           (b) pixd == pixs,   ~src --> src  (in-place)
 *           (c) pixd != pixs,   ~src --> input pixd
 *      (3) For clarity, if the case is known, use these patterns:
 *           (a) pixd = pixInvert(NULL, pixs);
 *           (b) pixInvert(pixs, pixs);
 *           (c) pixInvert(pixd, pixs);
 *}
function pixInvert( pixd, pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixOr()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs1,
 *                     different from pixs1)
 *              pixs1 (can be == pixd)
 *              pixs2 (must be != pixd)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This gives the union of two images with equal depth,
 *          aligning them to the the UL corner.  pixs1 and pixs2
 *          need not have the same width and height.
 *      (2) There are 3 cases:
 *            (a) pixd == null,   (src1 | src2) --> new pixd
 *            (b) pixd == pixs1,  (src1 | src2) --> src1  (in-place)
 *            (c) pixd != pixs1,  (src1 | src2) --> input pixd
 *      (3) For clarity, if the case is known, use these patterns:
 *            (a) pixd = pixOr(NULL, pixs1, pixs2);
 *            (b) pixOr(pixs1, pixs1, pixs2);
 *            (c) pixOr(pixd, pixs1, pixs2);
 *      (4) The size of the result is determined by pixs1.
 *      (5) The depths of pixs1 and pixs2 must be equal.
 *      (6) Note carefully that the order of pixs1 and pixs2 only matters
 *          for the in-place case.  For in-place, you must have
 *          pixd == pixs1.  Setting pixd == pixs2 gives an incorrect
 *          result: the copy puts pixs1 image data in pixs2, and
 *          the rasterop is then between pixs2 and pixs2 (a no-op).
 *}
function pixOr( pixd, pixs1, pixs2: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixAnd()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs1,
 *                     different from pixs1)
 *              pixs1 (can be == pixd)
 *              pixs2 (must be != pixd)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This gives the intersection of two images with equal depth,
 *          aligning them to the the UL corner.  pixs1 and pixs2
 *          need not have the same width and height.
 *      (2) There are 3 cases:
 *            (a) pixd == null,   (src1 & src2) --> new pixd
 *            (b) pixd == pixs1,  (src1 & src2) --> src1  (in-place)
 *            (c) pixd != pixs1,  (src1 & src2) --> input pixd
 *      (3) For clarity, if the case is known, use these patterns:
 *            (a) pixd = pixAnd(NULL, pixs1, pixs2);
 *            (b) pixAnd(pixs1, pixs1, pixs2);
 *            (c) pixAnd(pixd, pixs1, pixs2);
 *      (4) The size of the result is determined by pixs1.
 *      (5) The depths of pixs1 and pixs2 must be equal.
 *      (6) Note carefully that the order of pixs1 and pixs2 only matters
 *          for the in-place case.  For in-place, you must have
 *          pixd == pixs1.  Setting pixd == pixs2 gives an incorrect
 *          result: the copy puts pixs1 image data in pixs2, and
 *          the rasterop is then between pixs2 and pixs2 (a no-op).
 *}
function pixAnd( pixd, pixs1, pixs2: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixXor()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs1,
 *                     different from pixs1)
 *              pixs1 (can be == pixd)
 *              pixs2 (must be != pixd)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This gives the XOR of two images with equal depth,
 *          aligning them to the the UL corner.  pixs1 and pixs2
 *          need not have the same width and height.
 *      (2) There are 3 cases:
 *            (a) pixd == null,   (src1 ^ src2) --> new pixd
 *            (b) pixd == pixs1,  (src1 ^ src2) --> src1  (in-place)
 *            (c) pixd != pixs1,  (src1 ^ src2) --> input pixd
 *      (3) For clarity, if the case is known, use these patterns:
 *            (a) pixd = pixXor(NULL, pixs1, pixs2);
 *            (b) pixXor(pixs1, pixs1, pixs2);
 *            (c) pixXor(pixd, pixs1, pixs2);
 *      (4) The size of the result is determined by pixs1.
 *      (5) The depths of pixs1 and pixs2 must be equal.
 *      (6) Note carefully that the order of pixs1 and pixs2 only matters
 *          for the in-place case.  For in-place, you must have
 *          pixd == pixs1.  Setting pixd == pixs2 gives an incorrect
 *          result: the copy puts pixs1 image data in pixs2, and
 *          the rasterop is then between pixs2 and pixs2 (a no-op).
 *}
function pixXor( pixd, pixs1, pixs2: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixSubtract()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs1,
 *                     equal to pixs2, or different from both pixs1 and pixs2)
 *              pixs1 (can be == pixd)
 *              pixs2 (can be == pixd)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This gives the set subtraction of two images with equal depth,
 *          aligning them to the the UL corner.  pixs1 and pixs2
 *          need not have the same width and height.
 *      (2) Source pixs2 is always subtracted from source pixs1.
 *          The result is
 *                  pixs1 \ pixs2 = pixs1 & (~pixs2)
 *      (3) There are 4 cases:
 *            (a) pixd == null,   (src1 - src2) --> new pixd
 *            (b) pixd == pixs1,  (src1 - src2) --> src1  (in-place)
 *            (c) pixd == pixs2,  (src1 - src2) --> src2  (in-place)
 *            (d) pixd != pixs1 && pixd != pixs2),
 *                                 (src1 - src2) --> input pixd
 *      (4) For clarity, if the case is known, use these patterns:
 *            (a) pixd = pixSubtract(NULL, pixs1, pixs2);
 *            (b) pixSubtract(pixs1, pixs1, pixs2);
 *            (c) pixSubtract(pixs2, pixs1, pixs2);
 *            (d) pixSubtract(pixd, pixs1, pixs2);
 *      (5) The size of the result is determined by pixs1.
 *      (6) The depths of pixs1 and pixs2 must be equal.
 *}
function pixSubtract( pixd, pixs1, pixs2: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                         Pixel counting                      *
 *-------------------------------------------------------------*}

{*!
 *  pixZero()
 *
 *      Input:  pix (all depths; not colormapped)
 *              &empty  (<return> 1 if all bits in image are 0; 0 otherwise)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) For a binary image, if there are no fg (black) pixels, empty = 1.
 *      (2) For a grayscale image, if all pixels are black (0), empty = 1.
 *      (3) For an RGB image, if all 4 components in every pixel is 0,
 *          empty = 1.
 *}
function pixZero( pix: PLPix; pempty: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixForegroundFraction()
 *
 *      Input:  pix (1 bpp)
 *              &fract (<return> fraction of ON pixels)
 *      Return: 0 if OK; 1 on error
 *}
function pixForegroundFraction( pix: PLPix; pfract: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCountPixels()
 *
 *      Input:  pix (1 bpp)
 *              &count (<return> count of ON pixels)
 *              tab8  (<optional> 8-bit pixel lookup table)
 *      Return: 0 if OK; 1 on error
 *}
function pixCountPixels( pix: PLPix; pcount, tab8: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixaCountPixels()
 *
 *      Input:  pixa (array of 1 bpp pix)
 *      Return: na of ON pixels in each pix, or null on error
 *}
function pixaCountPixels( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixCountPixelsInRow()
 *
 *      Input:  pix (1 bpp)
 *              row number
 *              &count (<return> sum of ON pixels in raster line)
 *              tab8  (<optional> 8-bit pixel lookup table)
 *      Return: 0 if OK; 1 on error
 *}
function pixCountPixelsInRow( pix: PLPix; row: LongInt; pcount, tab8: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCountPixelsByRow()
 *
 *      Input:  pix (1 bpp)
 *              tab8  (<optional> 8-bit pixel lookup table)
 *      Return: na of counts, or null on error
 *}
function pixCountPixelsByRow( pix: PLPix; tab8: PLongint ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixCountPixelsByColumn()
 *
 *      Input:  pix (1 bpp)
 *      Return: na of counts in each column, or null on error
 *}
function pixCountPixelsByColumn( pix: PLPix ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixSumPixelsByRow()
 *
 *      Input:  pix (1, 8 or 16 bpp; no colormap)
 *              tab8  (<optional> lookup table for 1 bpp; use null for 8 bpp)
 *      Return: na of pixel sums by row, or null on error
 *
 *  Notes:
 *      (1) To resample for a bin size different from 1, use
 *          numaUniformSampling() on the result of this function.
 *}
function pixSumPixelsByRow( pix: PLPix; tab8: PLongint ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixSumPixelsByColumn()
 *
 *      Input:  pix (1, 8 or 16 bpp; no colormap)
 *      Return: na of pixel sums by column, or null on error
 *
 *  Notes:
 *      (1) To resample for a bin size different from 1, use
 *          numaUniformSampling() on the result of this function.
 *}
function pixSumPixelsByColumn( pix: PLPix ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixThresholdPixelSum()
 *
 *      Input:  pix (1 bpp)
 *              threshold
 *              &above (<return> 1 if above threshold;
 *                               0 if equal to or less than threshold)
 *              tab8  (<optional> 8-bit pixel lookup table)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This sums the ON pixels and returns immediately if the count
 *          goes above threshold.  It is therefore more efficient
 *          for matching images (by running this function on the xor of
 *          the 2 images) than using pixCountPixels(), which counts all
 *          pixels before returning.
 *}
function pixThresholdPixelSum( pix: PLPix; thresh: LongInt; pabove, tab8: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  makePixelSumTab8()
 *
 *      Input:  void
 *      Return: table of 256 l_int32, or null on error
 *
 *  Notes:
 *      (1) This table of integers gives the number of 1 bits
 *          in the 8 bit index.
 *}
function makePixelSumTab8 : PLongint; cdecl; external LIBLEPT;


{*!
 *  makePixelCentroidTab8()
 *
 *      Input:  void
 *      Return: table of 256 l_int32, or null on error
 *
 *  Notes:
 *      (1) This table of integers gives the centroid weight of the 1 bits
 *          in the 8 bit index.  In other words, if sumtab is obtained by
 *          makePixelSumTab8, and centroidtab is obtained by
 *          makePixelCentroidTab8, then, for 1 <= i <= 255,
 *          centroidtab[i] / (float)sumtab[i]
 *          is the centroid of the 1 bits in the 8-bit index i, where the
 *          MSB is considered to have position 0 and the LSB is considered
 *          to have position 7.
 *}
function makePixelCentroidTab8 : PLongint; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *        Pixel counting in gray and colormapped images        *
 *-------------------------------------------------------------*}

{*!
 *  pixCountArbInRect()
 *
 *      Input:  pixs (8 bpp, or colormapped)
 *              box (<optional>) over which count is made;
 *                   use entire image null)
 *              val (pixel value to count)
 *              factor (subsampling factor; integer >= 1)
 *              &count (<return> count; estimate it if factor > 1)
 *      Return: na (histogram), or null on error
 *
 *  Notes:
 *      (1) If pixs is cmapped, @val is compared to the colormap index;
 *          otherwise, @val is compared to the grayscale value.
 *      (2) Set the subsampling @factor > 1 to reduce the amount of computation.
 *          If @factor > 1, multiply the count by @factor * @factor.
 *}
function pixCountArbInRect( pixs: PLPix; box: PLBox; val, factor: LongInt; pcount: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                       Sum of pixel values                   *
 *-------------------------------------------------------------*}

{*!
 *  pixSumPixelValues()
 *
 *      Input:  pix (1, 2, 4, 8, 16, 32 bpp; not cmapped)
 *              box (<optional> if null, use entire image)
 *              &sum (<return> sum of pixel values in region)
 *      Return: 0 if OK; 1 on error
 *}
function pixSumPixelValues( pix: PLPix; box: PLBox; psum: PDouble ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *              Mirrored tiling of a smaller image             *
 *-------------------------------------------------------------*}

{*!
 *  pixMirroredTiling()
 *
 *      Input:  pixs (8 or 32 bpp, small tile; to be replicated)
 *              w, h (dimensions of output pix)
 *      Return: pixd (usually larger pix, mirror-tiled with pixs),
 *              or null on error
 *
 *  Notes:
 *      (1) This uses mirrored tiling, where each row alternates
 *          with LR flips and every column alternates with TB
 *          flips, such that the result is a tiling with identical
 *          2 x 2 tiles, each of which is composed of these transforms:
 *                  -----------------
 *                  | 1    |  LR    |
 *                  -----------------
 *                  | TB   |  LR/TB |
 *                  -----------------
 *}
function pixMirroredTiling( pixs: PLPix; w, h: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  findTilePatchCenter()
 *
 *      Input:  pixs (8 or 16 bpp; distance function of a binary mask)
 *              box (region of pixs to search around)
 *              searchdir (L_HORIZ or L_VERT; direction to search)
 *              targdist (desired distance of selected patch center from fg)
 *              &dist (<return> actual distance of selected location)
 *              &xc, &yc (<return> location of selected patch center)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This looks for a patch of non-masked image, that is outside
 *          but near the input box.  The input pixs is a distance
 *          function giving the distance from the fg in a binary mask.
 *      (2) The target distance implicitly specifies a desired size
 *          for the patch.  The location of the center of the patch,
 *          and the actual distance from fg are returned.
 *      (3) If the target distance is larger than 255, a 16-bit distance
 *          transform is input.
 *      (4) It is assured that a square centered at (xc, yc) and of
 *          size 'dist' will not intersect with the fg of the binary
 *          mask that was used to generate pixs.
 *      (5) We search away from the component, in approximately
 *          the center 1/3 of its dimension.  This gives a better chance
 *          of finding patches that are close to the component.
 *}

