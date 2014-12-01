{ This file is an automated translation to pascal of adaptmap.c               }
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
 *  adaptmap.c
 *
 *  ===================================================================
 *  Image binarization algorithms are found in:
 *     grayquant.c:   standard, simple, general grayscale quantization
 *     adaptmap.c:    local adaptive; mostly gray-to-gray in preparation
 *                    for binarization
 *     binarize.c:    special binarization methods, locally adaptive.
 *  ===================================================================
 *
 *      Adaptive background normalization (top-level functions)
 *          PIX       *pixBackgroundNormSimple()     8 and 32 bpp
 *          PIX       *pixBackgroundNorm()           8 and 32 bpp
 *          PIX       *pixBackgroundNormMorph()      8 and 32 bpp
 *
 *      Arrays of inverted background values for normalization (16 bpp)
 *          l_int32    pixBackgroundNormGrayArray()   8 bpp input
 *          l_int32    pixBackgroundNormRGBArrays()   32 bpp input
 *          l_int32    pixBackgroundNormGrayArrayMorph()   8 bpp input
 *          l_int32    pixBackgroundNormRGBArraysMorph()   32 bpp input
 *
 *      Measurement of local background
 *          l_int32    pixGetBackgroundGrayMap()        8 bpp
 *          l_int32    pixGetBackgroundRGBMap()         32 bpp
 *          l_int32    pixGetBackgroundGrayMapMorph()   8 bpp
 *          l_int32    pixGetBackgroundRGBMapMorph()    32 bpp
 *          l_int32    pixFillMapHoles()
 *          PIX       *pixExtendByReplication()         8 bpp
 *          l_int32    pixSmoothConnectedRegions()      8 bpp
 *
 *      Measurement of local foreground
 *          l_int32    pixGetForegroundGrayMap()        8 bpp
 *
 *      Generate inverted background map for each component
 *          PIX       *pixGetInvBackgroundMap()   16 bpp
 *
 *      Apply inverse background map to image
 *          PIX       *pixApplyInvBackgroundGrayMap()   8 bpp
 *          PIX       *pixApplyInvBackgroundRGBMap()    32 bpp
 *
 *      Apply variable map
 *          PIX       *pixApplyVariableGrayMap()        8 bpp
 *
 *      Non-adaptive (global) mapping
 *          PIX       *pixGlobalNormRGB()               32 bpp or cmapped
 *          PIX       *pixGlobalNormNoSatRGB()          32 bpp
 *
 *      Adaptive threshold spread normalization
 *          l_int32    pixThresholdSpreadNorm()         8 bpp
 *
 *      Adaptive background normalization (flexible adaptaption)
 *          PIX       *pixBackgroundNormFlex()          8 bpp
 *
 *      Adaptive contrast normalization
 *          PIX             *pixContrastNorm()          8 bpp
 *          l_int32          pixMinMaxTiles()
 *          l_int32          pixSetLowContrast()
 *          PIX             *pixLinearTRCTiled()
 *          static l_int32  *iaaGetLinearTRC()
 *
 *  Background normalization is done by generating a reduced map (or set
 *  of maps) representing the estimated background value of the
 *  input image, and using this to shift the pixel values so that
 *  this background value is set to some constant value.
 *
 *  Specifically, normalization has 3 steps:
 *    (1) Generate a background map at a reduced scale.
 *    (2) Make the array of inverted background values by inverting
 *        the map.  The result is an array of local multiplicative factors.
 *    (3) Apply this inverse background map to the image
 *
 *  The inverse background arrays can be generated in two different ways here:
 *    (1) Remove the 'foreground' pixels and average over the remaining
 *        pixels in each tile.  Propagate values into tiles where
 *        values have not been assigned, either because there was not
 *        enough background in the tile or because the tile is covered
 *        by a foreground region described by an image mask.
 *        After the background map is made, the inverse map is generated by
 *        smoothing over some number of adjacent tiles
 *        (block convolution) and then inverting.
 *    (2) Remove the foreground pixels using a morphological closing
 *        on a subsampled version of the image.  Propagate values
 *        into pixels covered by an optional image mask.  Invert the
 *        background map without preconditioning by convolutional smoothing.
 *
 *  Note: Several of these functions make an implicit assumption about RGB
 *        component ordering.
 *
 *  Other methods for adaptively normalizing the image are also given here.
 *
 *  (1) pixThresholdSpreadNorm() computes a local threshold over the image
 *      and normalizes the input pixel values so that this computed threshold
 *      is a constant across the entire image.
 *
 *  (2) pixContrastNorm() computes and applies a local TRC so that the
 *      local dynamic range is expanded to the full 8 bits, where the
 *      darkest pixels are mapped to 0 and the lightest to 255.  This is
 *      useful for improving the appearance of pages with very light
 *      foreground or very dark background, and where the local TRC
 *      function doesn't change rapidly with position.
 *}

    {* Default input parameters for pixBackgroundNormSimple()
     * Note:
     *    (1) mincount must never exceed the tile area (width * height)
     *    (2) bgval must be sufficiently below 255 to avoid accidental
     *        saturation; otherwise it should be large to avoid
     *        shrinking the dynamic range
     *    (3) results should otherwise not be sensitive to these values
     *}
const DEFAULT_TILE_WIDTH : LongInt = 10;     
const DEFAULT_TILE_HEIGHT : LongInt = 15;     
const DEFAULT_FG_THRESHOLD : LongInt = 60;     
const DEFAULT_MIN_COUNT : LongInt = 40;     
const DEFAULT_BG_VAL : LongInt = 200;     
const DEFAULT_X_SMOOTH_SIZE : LongInt = 2;     
const DEFAULT_Y_SMOOTH_SIZE : LongInt = 1;     

{* ~NO_CONSOLE_IO *}

{*------------------------------------------------------------------*
 *                Adaptive background normalization                 *
 *------------------------------------------------------------------*}

{*!
 *  pixBackgroundNormSimple()
 *
 *      Input:  pixs (8 bpp grayscale or 32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              pixg (<optional> 8 bpp grayscale version; can be null)
 *      Return: pixd (8 bpp or 32 bpp rgb), or null on error
 *
 *  Notes:
 *    (1) This is a simplified interface to pixBackgroundNorm(),
 *        where seven parameters are defaulted.
 *    (2) The input image is either grayscale or rgb.
 *    (3) See pixBackgroundNorm() for usage and function.
 *}
function pixBackgroundNormSimple( pixs, pixim, pixg: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBackgroundNorm()
 *
 *      Input:  pixs (8 bpp grayscale or 32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              pixg (<optional> 8 bpp grayscale version; can be null)
 *              sx, sy (tile size in pixels)
 *              thresh (threshold for determining foreground)
 *              mincount (min threshold on counts in a tile)
 *              bgval (target bg val; typ. > 128)
 *              smoothx (half-width of block convolution kernel width)
 *              smoothy (half-width of block convolution kernel height)
 *      Return: pixd (8 bpp or 32 bpp rgb), or null on error
 *
 *  Notes:
 *    (1) This is a top-level interface for normalizing the image intensity
 *        by mapping the image so that the background is near the input
 *        value 'bgval'.
 *    (2) The input image is either grayscale or rgb.
 *    (3) For each component in the input image, the background value
 *        in each tile is estimated using the values in the tile that
 *        are not part of the foreground, where the foreground is
 *        determined by the input 'thresh' argument.
 *    (4) An optional binary mask can be specified, with the foreground
 *        pixels typically over image regions.  The resulting background
 *        map values will be determined by surrounding pixels that are
 *        not under the mask foreground.  The origin (0,0) of this mask
 *        is assumed to be aligned with the origin of the input image.
 *        This binary mask must not fully cover pixs, because then there
 *        will be no pixels in the input image available to compute
 *        the background.
 *    (5) An optional grayscale version of the input pixs can be supplied.
 *        The only reason to do this is if the input is RGB and this
 *        grayscale version can be used elsewhere.  If the input is RGB
 *        and this is not supplied, it is made internally using only
 *        the green component, and destroyed after use.
 *    (6) The dimensions of the pixel tile (sx, sy) give the amount by
 *        by which the map is reduced in size from the input image.
 *    (7) The threshold is used to binarize the input image, in order to
 *        locate the foreground components.  If this is set too low,
 *        some actual foreground may be used to determine the maps;
 *        if set too high, there may not be enough background
 *        to determine the map values accurately.  Typically, it's
 *        better to err by setting the threshold too high.
 *    (8) A 'mincount' threshold is a minimum count of pixels in a
 *        tile for which a background reading is made, in order for that
 *        pixel in the map to be valid.  This number should perhaps be
 *        at least 1/3 the size of the tile.
 *    (9) A 'bgval' target background value for the normalized image.  This
 *        should be at least 128.  If set too close to 255, some
 *        clipping will occur in the result.
 *    (10) Two factors, 'smoothx' and 'smoothy', are input for smoothing
 *        the map.  Each low-pass filter kernel dimension is
 *        is 2 * (smoothing factor) + 1, so a
 *        value of 0 means no smoothing. A value of 1 or 2 is recommended.
 *}
function pixBackgroundNorm( pixs, pixim, pixg: PLPix; sx, sy, thresh, mincount, bgval, smoothx, smoothy: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBackgroundNormMorph()
 *
 *      Input:  pixs (8 bpp grayscale or 32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              reduction (at which morph closings are done; between 2 and 16)
 *              size (of square Sel for the closing; use an odd number)
 *              bgval (target bg val; typ. > 128)
 *      Return: pixd (8 bpp), or null on error
 *
 *  Notes:
 *    (1) This is a top-level interface for normalizing the image intensity
 *        by mapping the image so that the background is near the input
 *        value 'bgval'.
 *    (2) The input image is either grayscale or rgb.
 *    (3) For each component in the input image, the background value
 *        is estimated using a grayscale closing; hence the 'Morph'
 *        in the function name.
 *    (4) An optional binary mask can be specified, with the foreground
 *        pixels typically over image regions.  The resulting background
 *        map values will be determined by surrounding pixels that are
 *        not under the mask foreground.  The origin (0,0) of this mask
 *        is assumed to be aligned with the origin of the input image.
 *        This binary mask must not fully cover pixs, because then there
 *        will be no pixels in the input image available to compute
 *        the background.
 *    (5) The map is computed at reduced size (given by 'reduction')
 *        from the input pixs and optional pixim.  At this scale,
 *        pixs is closed to remove the background, using a square Sel
 *        of odd dimension.  The product of reduction * size should be
 *        large enough to remove most of the text foreground.
 *    (6) No convolutional smoothing needs to be done on the map before
 *        inverting it.
 *    (7) A 'bgval' target background value for the normalized image.  This
 *        should be at least 128.  If set too close to 255, some
 *        clipping will occur in the result.
 *}
function pixBackgroundNormMorph( pixs, pixim: PLPix; reduction, size, bgval: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------------------*
 *      Arrays of inverted background values for normalization             *
 *-------------------------------------------------------------------------*
 *  Notes for these four functions:                                        *
 *      (1) They are useful if you need to save the actual mapping array.  *
 *      (2) They could be used in the top-level functions but are          *
 *          not because their use makes those functions less clear.        *
 *      (3) Each component in the input pixs generates a 16 bpp pix array. *
 *-------------------------------------------------------------------------*}

{*!
 *  pixBackgroundNormGrayArray()
 *
 *      Input:  pixs (8 bpp grayscale)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              sx, sy (tile size in pixels)
 *              thresh (threshold for determining foreground)
 *              mincount (min threshold on counts in a tile)
 *              bgval (target bg val; typ. > 128)
 *              smoothx (half-width of block convolution kernel width)
 *              smoothy (half-width of block convolution kernel height)
 *              &pixd (<return> 16 bpp array of inverted background value)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *    (1) See notes in pixBackgroundNorm().
 *    (2) This returns a 16 bpp pix that can be used by
 *        pixApplyInvBackgroundGrayMap() to generate a normalized version
 *        of the input pixs.
 *}
function pixBackgroundNormGrayArray( pixs, pixim: PLPix; sx, sy, thresh, mincount, bgval, smoothx, smoothy: LongInt; ppixd: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixBackgroundNormRGBArrays()
 *
 *      Input:  pixs (32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              pixg (<optional> 8 bpp grayscale version; can be null)
 *              sx, sy (tile size in pixels)
 *              thresh (threshold for determining foreground)
 *              mincount (min threshold on counts in a tile)
 *              bgval (target bg val; typ. > 128)
 *              smoothx (half-width of block convolution kernel width)
 *              smoothy (half-width of block convolution kernel height)
 *              &pixr (<return> 16 bpp array of inverted R background value)
 *              &pixg (<return> 16 bpp array of inverted G background value)
 *              &pixb (<return> 16 bpp array of inverted B background value)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *    (1) See notes in pixBackgroundNorm().
 *    (2) This returns a set of three 16 bpp pix that can be used by
 *        pixApplyInvBackgroundGrayMap() to generate a normalized version
 *        of each component of the input pixs.
 *}
function pixBackgroundNormRGBArrays( pixs, pixim, pixg: PLPix; sx, sy, thresh, mincount, bgval, smoothx, smoothy: LongInt; ppixr, ppixg, ppixb: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixBackgroundNormGrayArrayMorph()
 *
 *      Input:  pixs (8 bpp grayscale)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              reduction (at which morph closings are done; between 2 and 16)
 *              size (of square Sel for the closing; use an odd number)
 *              bgval (target bg val; typ. > 128)
 *              &pixd (<return> 16 bpp array of inverted background value)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *    (1) See notes in pixBackgroundNormMorph().
 *    (2) This returns a 16 bpp pix that can be used by
 *        pixApplyInvBackgroundGrayMap() to generate a normalized version
 *        of the input pixs.
 *}
function pixBackgroundNormGrayArrayMorph( pixs, pixim: PLPix; reduction, size, bgval: LongInt; ppixd: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixBackgroundNormRGBArraysMorph()
 *
 *      Input:  pixs (32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              reduction (at which morph closings are done; between 2 and 16)
 *              size (of square Sel for the closing; use an odd number)
 *              bgval (target bg val; typ. > 128)
 *              &pixr (<return> 16 bpp array of inverted R background value)
 *              &pixg (<return> 16 bpp array of inverted G background value)
 *              &pixb (<return> 16 bpp array of inverted B background value)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *    (1) See notes in pixBackgroundNormMorph().
 *    (2) This returns a set of three 16 bpp pix that can be used by
 *        pixApplyInvBackgroundGrayMap() to generate a normalized version
 *        of each component of the input pixs.
 *}
function pixBackgroundNormRGBArraysMorph( pixs, pixim: PLPix; reduction, size, bgval: LongInt; ppixr, ppixg, ppixb: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                 Measurement of local background                  *
 *------------------------------------------------------------------*}

{*!
 *  pixGetBackgroundGrayMap()
 *
 *      Input:  pixs (8 bpp grayscale; not cmapped)
 *              pixim (<optional> 1 bpp 'image' mask; can be null; it
 *                     should not have all foreground pixels)
 *              sx, sy (tile size in pixels)
 *              thresh (threshold for determining foreground)
 *              mincount (min threshold on counts in a tile)
 *              &pixd (<return> 8 bpp grayscale map)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The background is measured in regions that don't have
 *          images.  It is then propagated into the image regions,
 *          and finally smoothed in each image region.
 *}
function pixGetBackgroundGrayMap( pixs, pixim: PLPix; sx, sy, thresh, mincount: LongInt; ppixd: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetBackgroundRGBMap()
 *
 *      Input:  pixs (32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null; it
 *                     should not have all foreground pixels)
 *              pixg (<optional> 8 bpp grayscale version; can be null)
 *              sx, sy (tile size in pixels)
 *              thresh (threshold for determining foreground)
 *              mincount (min threshold on counts in a tile)
 *              &pixmr, &pixmg, &pixmb (<return> rgb maps)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) If pixg, which is a grayscale version of pixs, is provided,
 *          use this internally to generate the foreground mask.
 *          Otherwise, a grayscale version of pixs will be generated
 *          from the green component only, used, and destroyed.
 *}
function pixGetBackgroundRGBMap( pixs, pixim, pixg: PLPix; sx, sy, thresh, mincount: LongInt; ppixmr, ppixmg, ppixmb: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetBackgroundGrayMapMorph()
 *
 *      Input:  pixs (8 bpp grayscale; not cmapped)
 *              pixim (<optional> 1 bpp 'image' mask; can be null; it
 *                     should not have all foreground pixels)
 *              reduction (factor at which closing is performed)
 *              size (of square Sel for the closing; use an odd number)
 *              &pixm (<return> grayscale map)
 *      Return: 0 if OK, 1 on error
 *}
function pixGetBackgroundGrayMapMorph( pixs, pixim: PLPix; reduction, size: LongInt; ppixm: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetBackgroundRGBMapMorph()
 *
 *      Input:  pixs (32 bpp rgb)
 *              pixim (<optional> 1 bpp 'image' mask; can be null; it
 *                     should not have all foreground pixels)
 *              reduction (factor at which closing is performed)
 *              size (of square Sel for the closing; use an odd number)
 *              &pixmr (<return> red component map)
 *              &pixmg (<return> green component map)
 *              &pixmb (<return> blue component map)
 *      Return: 0 if OK, 1 on error
 *}
function pixGetBackgroundRGBMapMorph( pixs, pixim: PLPix; reduction, size: LongInt; ppixmr, ppixmg, ppixmb: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixFillMapHoles()
 *
 *      Input:  pix (8 bpp; a map, with one pixel for each tile in
 *              a larger image)
 *              nx (number of horizontal pixel tiles that are entirely
 *                  covered with pixels in the original source image)
 *              ny (ditto for the number of vertical pixel tiles)
 *              filltype (L_FILL_WHITE or L_FILL_BLACK)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is an in-place operation on pix (the map).  pix is
 *          typically a low-resolution version of some other image
 *          from which it was derived, where each pixel in pix
 *          corresponds to a rectangular tile (say, m x n) of pixels
 *          in the larger image.  All we need to know about the larger
 *          image is whether or not the rightmost column and bottommost
 *          row of pixels in pix correspond to tiles that are
 *          only partially covered by pixels in the larger image.
 *      (2) Typically, some number of pixels in the input map are
 *          not known, and their values must be determined by near
 *          pixels that are known.  These unknown pixels are the 'holes'.
 *          They can take on only two values, 0 and 255, and the
 *          instruction about which to fill is given by the filltype flag.
 *      (3) The "holes" can come from two sources.  The first is when there
 *          are not enough foreground or background pixels in a tile;
 *          the second is when a tile is at least partially covered
 *          by an image mask.  If we're filling holes in a fg mask,
 *          the holes are initialized to black (0) and use L_FILL_BLACK.
 *          For filling holes in a bg mask, initialize the holes to
 *          white (255) and use L_FILL_WHITE.
 *      (4) If w is the map width, nx = w or nx = w - 1; ditto for h and ny.
 *}
function pixFillMapHoles( pix: PLPix; nx, ny, filltype: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixExtendByReplication()
 *
 *      Input:  pixs (8 bpp)
 *              addw (number of extra pixels horizontally to add)
 *              addh (number of extra pixels vertically to add)
 *      Return: pixd (extended with replicated pixel values), or null on error
 *
 *  Notes:
 *      (1) The pixel values are extended to the left and down, as required.
 *}
function pixExtendByReplication( pixs: PLPix; addw, addh: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixSmoothConnectedRegions()
 *
 *      Input:  pixs (8 bpp grayscale; no colormap)
 *              pixm (<optional> 1 bpp; if null, this is a no-op)
 *              factor (subsampling factor for getting average; >= 1)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The pixels in pixs corresponding to those in each
 *          8-connected region in the mask are set to the average value.
 *      (2) This is required for adaptive mapping to avoid the
 *          generation of stripes in the background map, due to
 *          variations in the pixel values near the edges of mask regions.
 *      (3) This function is optimized for background smoothing, where
 *          there are a relatively small number of components.  It will
 *          be inefficient if used where there are many small components.
 *}
function pixSmoothConnectedRegions( pixs, pixm: PLPix; factor: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                 Measurement of local foreground                  *
 *------------------------------------------------------------------*}

{* Not working properly: do not use *}

{*!
 *  pixGetForegroundGrayMap()
 *
 *      Input:  pixs (8 bpp)
 *              pixim (<optional> 1 bpp 'image' mask; can be null)
 *              sx, sy (src tile size, in pixels)
 *              thresh (threshold for determining foreground)
 *              &pixd (<return> 8 bpp grayscale map)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Each (sx, sy) tile of pixs gets mapped to one pixel in pixd.
 *      (2) pixd is the estimate of the fg (darkest) value within each tile.
 *      (3) All pixels in pixd that are in 'image' regions, as specified
 *          by pixim, are given the background value 0.
 *      (4) For pixels in pixd that can't directly be given a fg value,
 *          the value is inferred by propagating from neighboring pixels.
 *      (5) In practice, pixd can be used to normalize the fg, and
 *          it can be done after background normalization.
 *      (6) The overall procedure is:
 *            - reduce 2x by sampling
 *            - paint all 'image' pixels white, so that they don't
 *              participate in the Min reduction
 *            - do a further (sx, sy) Min reduction -- think of
 *              it as a large opening followed by subsampling by the
 *              reduction factors
 *            - threshold the result to identify fg, and set the
 *              bg pixels to 255 (these are 'holes')
 *            - fill holes by propagation from fg values
 *            - replicatively expand by 2x, arriving at the final
 *              resolution of pixd
 *            - smooth with a 17x17 kernel
 *            - paint the 'image' regions black
 *}
function pixGetForegroundGrayMap( pixs, pixim: PLPix; sx, sy, thresh: LongInt; ppixd: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{* Not working properly: do not use *}

{*------------------------------------------------------------------*
 *                  Generate inverted background map                *
 *------------------------------------------------------------------*}

{*!
 *  pixGetInvBackgroundMap()
 *
 *      Input:  pixs (8 bpp grayscale; no colormap)
 *              bgval (target bg val; typ. > 128)
 *              smoothx (half-width of block convolution kernel width)
 *              smoothy (half-width of block convolution kernel height)
 *      Return: pixd (16 bpp), or null on error
 *
 *  Note:
 *     - bgval should typically be > 120 and < 240
 *     - pixd is a normalization image; the original image is
 *       multiplied by pixd and the result is divided by 256.
 *}
function pixGetInvBackgroundMap( pixs: PLPix; bgval, smoothx, smoothy: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                    Apply background map to image                 *
 *------------------------------------------------------------------*}

{*!
 *  pixApplyInvBackgroundGrayMap()
 *
 *      Input:  pixs (8 bpp grayscale; no colormap)
 *              pixm (16 bpp, inverse background map)
 *              sx (tile width in pixels)
 *              sy (tile height in pixels)
 *      Return: pixd (8 bpp), or null on error
 *}
function pixApplyInvBackgroundGrayMap( pixs, pixm: PLPix; sx, sy: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixApplyInvBackgroundRGBMap()
 *
 *      Input:  pixs (32 bpp rbg)
 *              pixmr (16 bpp, red inverse background map)
 *              pixmg (16 bpp, green inverse background map)
 *              pixmb (16 bpp, blue inverse background map)
 *              sx (tile width in pixels)
 *              sy (tile height in pixels)
 *      Return: pixd (32 bpp rbg), or null on error
 *}
function pixApplyInvBackgroundRGBMap( pixs, pixmr, pixmg, pixmb: PLPix; sx, sy: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                         Apply variable map                       *
 *------------------------------------------------------------------*}

{*!
 *  pixApplyVariableGrayMap()
 *
 *      Input:  pixs (8 bpp)
 *              pixg (8 bpp, variable map)
 *              target (typ. 128 for threshold)
 *      Return: pixd (8 bpp), or null on error
 *
 *  Notes:
 *      (1) Suppose you have an image that you want to transform based
 *          on some photometric measurement at each point, such as the
 *          threshold value for binarization.  Representing the photometric
 *          measurement as an image pixg, you can threshold in input image
 *          using pixVarThresholdToBinary().  Alternatively, you can map
 *          the input image pointwise so that the threshold over the
 *          entire image becomes a constant, such as 128.  For example,
 *          if a pixel in pixg is 150 and the target is 128, the
 *          corresponding pixel in pixs is mapped linearly to a value
 *          (128/150) of the input value.  If the resulting mapped image
 *          pixd were then thresholded at 128, you would obtain the
 *          same result as a direct binarization using pixg with
 *          pixVarThresholdToBinary().
 *      (2) The sizes of pixs and pixg must be equal.
 *}
function pixApplyVariableGrayMap( pixs, pixg: PLPix; target: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                  Non-adaptive (global) mapping                   *
 *------------------------------------------------------------------*}

{*!
 *  pixGlobalNormRGB()
 *
 *      Input:  pixd (<optional> null, existing or equal to pixs)
 *              pixs (32 bpp rgb, or colormapped)
 *              rval, gval, bval (pixel values in pixs that are
 *                                linearly mapped to mapval)
 *              mapval (use 255 for mapping to white)
 *      Return: pixd (32 bpp rgb or colormapped), or null on error
 *
 *  Notes:
 *    (1) The value of pixd determines if the results are written to a
 *        new pix (use NULL), in-place to pixs (use pixs), or to some
 *        other existing pix.
 *    (2) This does a global normalization of an image where the
 *        r,g,b color components are not balanced.  Thus, white in pixs is
 *        represented by a set of r,g,b values that are not all 255.
 *    (3) The input values (rval, gval, bval) should be chosen to
 *        represent the gray color (mapval, mapval, mapval) in src.
 *        Thus, this function will map (rval, gval, bval) to that gray color.
 *    (4) Typically, mapval = 255, so that (rval, gval, bval)
 *        corresponds to the white point of src.  In that case, these
 *        parameters should be chosen so that few pixels have higher values.
 *    (5) In all cases, we do a linear TRC separately on each of the
 *        components, saturating at 255.
 *    (6) If the input pix is 8 bpp without a colormap, you can get
 *        this functionality with mapval = 255 by calling:
 *            pixGammaTRC(pixd, pixs, 1.0, 0, bgval);
 *        where bgval is the value you want to be mapped to 255.
 *        Or more generally, if you want bgval to be mapped to mapval:
 *            pixGammaTRC(pixd, pixs, 1.0, 0, 255 * bgval / mapval);
 *}
function pixGlobalNormRGB( pixd, pixs: PLPix; rval, gval, bval, mapval: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixGlobalNormNoSatRGB()
 *
 *      Input:  pixd (<optional> null, existing or equal to pixs)
 *              pixs (32 bpp rgb)
 *              rval, gval, bval (pixel values in pixs that are
 *                                linearly mapped to mapval; but see below)
 *              factor (subsampling factor; integer >= 1)
 *              rank (between 0.0 and 1.0; typ. use a value near 1.0)
 *      Return: pixd (32 bpp rgb), or null on error
 *
 *  Notes:
 *    (1) This is a version of pixGlobalNormRGB(), where the output
 *        intensity is scaled back so that a controlled fraction of
 *        pixel components is allowed to saturate.  See comments in
 *        pixGlobalNormRGB().
 *    (2) The value of pixd determines if the results are written to a
 *        new pix (use NULL), in-place to pixs (use pixs), or to some
 *        other existing pix.
 *    (3) This does a global normalization of an image where the
 *        r,g,b color components are not balanced.  Thus, white in pixs is
 *        represented by a set of r,g,b values that are not all 255.
 *    (4) The input values (rval, gval, bval) can be chosen to be the
 *        color that, after normalization, becomes white background.
 *        For images that are mostly background, the closer these values
 *        are to the median component values, the closer the resulting
 *        background will be to gray, becoming white at the brightest places.
 *    (5) The mapval used in pixGlobalNormRGB() is computed here to
 *        avoid saturation of any component in the image (save for a
 *        fraction of the pixels given by the input rank value).
 *}
function pixGlobalNormNoSatRGB( pixd, pixs: PLPix; rval, gval, bval, factor: LongInt; rank: Single ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *              Adaptive threshold spread normalization             *
 *------------------------------------------------------------------*}

{*!
 *  pixThresholdSpreadNorm()
 *
 *      Input:  pixs (8 bpp grayscale; not colormapped)
 *              filtertype (L_SOBEL_EDGE or L_TWO_SIDED_EDGE);
 *              edgethresh (threshold on magnitude of edge filter; typ 10-20)
 *              smoothx, smoothy (half-width of convolution kernel applied to
 *                                spread threshold: use 0 for no smoothing)
 *              gamma (gamma correction; typ. about 0.7)
 *              minval  (input value that gives 0 for output; typ. -25)
 *              maxval  (input value that gives 255 for output; typ. 255)
 *              targetthresh (target threshold for normalization)
 *              &pixth (<optional return> computed local threshold value)
 *              &pixb (<optional return> thresholded normalized image)
 *              &pixd (<optional return> normalized image)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The basis of this approach is the use of seed spreading
 *          on a (possibly) sparse set of estimates for the local threshold.
 *          The resulting dense estimates are smoothed by convolution
 *          and used to either threshold the input image or normalize it
 *          with a local transformation that linearly maps the pixels so
 *          that the local threshold estimate becomes constant over the
 *          resulting image.  This approach is one of several that
 *          have been suggested (and implemented) by Ray Smith.
 *      (2) You can use either the Sobel or TwoSided edge filters.
 *          The results appear to be similar, using typical values
 *          of edgethresh in the rang 10-20.
 *      (3) To skip the trc enhancement, use gamma = 1.0, minval = 0
 *          and maxval = 255.
 *      (4) For the normalized image pixd, each pixel is linearly mapped
 *          in such a way that the local threshold is equal to targetthresh.
 *      (5) The full width and height of the convolution kernel
 *          are (2 * smoothx + 1) and (2 * smoothy + 1).
 *      (6) This function can be used with the pixtiling utility if the
 *          images are too large.  See pixOtsuAdaptiveThreshold() for
 *          an example of this.
 *}
function pixThresholdSpreadNorm( pixs: PLPix; filtertype, edgethresh, smoothx, smoothy: LongInt; gamma: Single; minval, maxval, targetthresh: LongInt; ppixth, ppixb, ppixd: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *      Adaptive background normalization (flexible adaptaption)    *
 *------------------------------------------------------------------*}

{*!
 *  pixBackgroundNormFlex()
 *
 *      Input:  pixs (8 bpp grayscale; not colormapped)
 *              sx, sy (desired tile dimensions; actual size may vary; use
 *                      values between 3 and 10)
 *              smoothx, smoothy (half-width of convolution kernel applied to
 *                                threshold array: use values between 1 and 3)
 *              delta (difference parameter in basin filling; use 0
 *                     to skip)
 *      Return: pixd (8 bpp, background-normalized), or null on error)
 *
 *  Notes:
 *      (1) This does adaptation flexibly to a quickly varying background.
 *          For that reason, all input parameters should be small.
 *      (2) sx and sy give the tile size; they should be in [5 - 7].
 *      (3) The full width and height of the convolution kernel
 *          are (2 * smoothx + 1) and (2 * smoothy + 1).  They
 *          should be in [1 - 2].
 *      (4) Basin filling is used to fill the large fg regions.  The
 *          parameter @delta measures the height that the black
 *          background is raised from the local minima.  By raising
 *          the background, it is possible to threshold the large
 *          fg regions to foreground.  If @delta is too large,
 *          bg regions will be lifted, causing thickening of
 *          the fg regions.  Use 0 to skip.
 *}
function pixBackgroundNormFlex( pixs: PLPix; sx, sy, smoothx, smoothy, delta: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                    Adaptive contrast normalization               *
 *------------------------------------------------------------------*}

{*!
 *  pixContrastNorm()
 *
 *      Input:  pixd (<optional> 8 bpp; null or equal to pixs)
 *              pixs (8 bpp grayscale; not colormapped)
 *              sx, sy (tile dimensions)
 *              mindiff (minimum difference to accept as valid)
 *              smoothx, smoothy (half-width of convolution kernel applied to
 *                                min and max arrays: use 0 for no smoothing)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This function adaptively attempts to expand the contrast
 *          to the full dynamic range in each tile.  If the contrast in
 *          a tile is smaller than @mindiff, it uses the min and max
 *          pixel values from neighboring tiles.  It also can use
 *          convolution to smooth the min and max values from
 *          neighboring tiles.  After all that processing, it is
 *          possible that the actual pixel values in the tile are outside
 *          the computed [min ... max] range for local contrast
 *          normalization.  Such pixels are taken to be at either 0
 *          (if below the min) or 255 (if above the max).
 *      (2) pixd can be equal to pixs (in-place operation) or
 *          null (makes a new pixd).
 *      (3) sx and sy give the tile size; they are typically at least 20.
 *      (4) mindiff is used to eliminate results for tiles where it is
 *          likely that either fg or bg is missing.  A value around 50
 *          or more is reasonable.
 *      (5) The full width and height of the convolution kernel
 *          are (2 * smoothx + 1) and (2 * smoothy + 1).  Some smoothing
 *          is typically useful, and we limit the smoothing half-widths
 *          to the range from 0 to 8.
 *      (6) A linear TRC (gamma = 1.0) is applied to increase the contrast
 *          in each tile.  The result can subsequently be globally corrected,
 *          by applying pixGammaTRC() with arbitrary values of gamma
 *          and the 0 and 255 points of the mapping.
 *}
function pixContrastNorm( pixd, pixs: PLPix; sx, sy, mindiff, smoothx, smoothy: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixMinMaxTiles()
 *
 *      Input:  pixs (8 bpp grayscale; not colormapped)
 *              sx, sy (tile dimensions)
 *              mindiff (minimum difference to accept as valid)
 *              smoothx, smoothy (half-width of convolution kernel applied to
 *                                min and max arrays: use 0 for no smoothing)
 *              &pixmin (<return> tiled minima)
 *              &pixmax (<return> tiled maxima)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This computes filtered and smoothed values for the min and
 *          max pixel values in each tile of the image.
 *      (2) See pixContrastNorm() for usage.
 *}
function pixMinMaxTiles( pixs: PLPix; sx, sy, mindiff, smoothx, smoothy: LongInt; ppixmin, ppixmax: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetLowContrast()
 *
 *      Input:  pixs1 (8 bpp)
 *              pixs2 (8 bpp)
 *              mindiff (minimum difference to accept as valid)
 *      Return: 0 if OK; 1 if no pixel diffs are large enough, or on error
 *
 *  Notes:
 *      (1) This compares corresponding pixels in pixs1 and pixs2.
 *          When they differ by less than @mindiff, set the pixel
 *          values to 0 in each.  Each pixel typically represents a tile
 *          in a larger image, and a very small difference between
 *          the min and max in the tile indicates that the min and max
 *          values are not to be trusted.
 *      (2) If contrast (pixel difference) detection is expected to fail,
 *          caller should check return value.
 *}
function pixSetLowContrast( pixs1, pixs2: PLPix; mindiff: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixLinearTRCTiled()
 *
 *      Input:  pixd (<optional> 8 bpp)
 *              pixs (8 bpp, not colormapped)
 *              sx, sy (tile dimensions)
 *              pixmin (pix of min values in tiles)
 *              pixmax (pix of max values in tiles)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) pixd can be equal to pixs (in-place operation) or
 *          null (makes a new pixd).
 *      (2) sx and sy give the tile size; they are typically at least 20.
 *      (3) pixmin and pixmax are generated by pixMinMaxTiles()
 *      (4) For each tile, this does a linear expansion of the dynamic
 *          range so that the min value in the tile becomes 0 and the
 *          max value in the tile becomes 255.
 *      (5) The LUTs that do the mapping are generated as needed
 *          and stored for reuse in an integer array within the ptr array iaa[].
 *}
function pixLinearTRCTiled( pixd, pixs: PLPix; sx, sy: LongInt; pixmin, pixmax: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  iaaGetLinearTRC()
 *
 *      Input:  iaa (bare array of ptrs to l_int32)
 *              diff (between min and max pixel values that are
 *                    to be mapped to 0 and 255)
 *      Return: ia (LUT with input (val - minval) and output a
 *                  value between 0 and 255)
 *}

