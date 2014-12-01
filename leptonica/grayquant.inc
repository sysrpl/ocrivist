{ This file is an automated translation to pascal of grayquant.c              }
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
 *  grayquant.c
 *
 *      Thresholding from 8 bpp to 1 bpp
 *
 *          Floyd-Steinberg dithering to binary
 *              PIX    *pixDitherToBinary()
 *              PIX    *pixDitherToBinarySpec()
 *
 *          Simple (pixelwise) binarization with fixed threshold
 *              PIX    *pixThresholdToBinary()
 *
 *          Binarization with variable threshold
 *              PIX    *pixVarThresholdToBinary()
 *
 *          Slower implementation of Floyd-Steinberg dithering, using LUTs
 *              PIX    *pixDitherToBinaryLUT()
 *
 *          Generate a binary mask from pixels of particular values
 *              PIX    *pixGenerateMaskByValue()
 *              PIX    *pixGenerateMaskByBand()
 *
 *      Thresholding from 8 bpp to 2 bpp
 *
 *          Dithering to 2 bpp
 *              PIX      *pixDitherTo2bpp()
 *              PIX      *pixDitherTo2bppSpec()
 *
 *          Simple (pixelwise) thresholding to 2 bpp with optional cmap
 *              PIX      *pixThresholdTo2bpp()
 *
 *      Simple (pixelwise) thresholding from 8 bpp to 4 bpp
 *              PIX      *pixThresholdTo4bpp()
 *
 *      Simple (pixelwise) quantization on 8 bpp grayscale
 *              PIX      *pixThresholdOn8bpp()
 *
 *      Arbitrary (pixelwise) thresholding from 8 bpp to 2, 4 or 8 bpp
 *              PIX      *pixThresholdGrayArb()
 *
 *      Quantization tables for linear thresholds of grayscale images
 *              l_int32  *makeGrayQuantIndexTable()
 *              l_int32  *makeGrayQuantTargetTable()
 *
 *      Quantization table for arbitrary thresholding of grayscale images
 *              l_int32   makeGrayQuantTableArb()
 *              l_int32   makeGrayQuantColormapArb()
 *
 *      Thresholding from 32 bpp rgb to 1 bpp
 *      (really color quantization, but it's better placed in this file)
 *              PIX      *pixGenerateMaskByBand32()
 *              PIX      *pixGenerateMaskByDiscr32()
 *
 *      Histogram-based grayscale quantization
 *              PIX      *pixGrayQuantFromHisto()
 *       static l_int32   numaFillCmapFromHisto()
 *
 *      Color quantize grayscale image using existing colormap
 *              PIX      *pixGrayQuantFromCmap()
 *}

{*------------------------------------------------------------------*
 *             Binarization by Floyd-Steinberg dithering            *
 *------------------------------------------------------------------*}

{*!
 *  pixDitherToBinary()
 *
 *      Input:  pixs
 *      Return: pixd (dithered binary), or null on error
 *
 *  The Floyd-Steinberg error diffusion dithering algorithm
 *  binarizes an 8 bpp grayscale image to a threshold of 128.
 *  If a pixel has a value above 127, it is binarized to white
 *  and the excess (below 255) is subtracted from three
 *  neighboring pixels in the fractions 3/8 to (i, j+1),
 *  3/8 to (i+1, j) and 1/4 to (i+1,j+1), truncating to 0
 *  if necessary.  Likewise, if it the pixel has a value
 *  below 128, it is binarized to black and the excess above 0
 *  is added to the neighboring pixels, truncating to 255 if necessary.
 *
 *  This function differs from straight dithering in that it allows
 *  clipping of grayscale to 0 or 255 if the values are
 *  sufficiently close, without distribution of the excess.
 *  This uses default values to specify the range of lower
 *  and upper values (near 0 and 255, rsp) that are clipped
 *  to black and white without propagating the excess.
 *  Not propagating the excess has the effect of reducing the
 *  snake patterns in parts of the image that are nearly black or white;
 *  however, it also prevents the attempt to reproduce gray for those values.
 *
 *  The implementation is straightforward.  It uses a pair of
 *  line buffers to avoid changing pixs.  It is about 2x faster
 *  than the implementation using LUTs.
 *}
function pixDitherToBinary( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixDitherToBinarySpec()
 *
 *      Input:  pixs
 *              lowerclip (lower clip distance to black; use 0 for default)
 *              upperclip (upper clip distance to white; use 0 for default)
 *      Return: pixd (dithered binary), or null on error
 *
 *  Notes:
 *      (1) See comments above in pixDitherToBinary() for details.
 *      (2) The input parameters lowerclip and upperclip specify the range
 *          of lower and upper values (near 0 and 255, rsp) that are
 *          clipped to black and white without propagating the excess.
 *          For that reason, lowerclip and upperclip should be small numbers.
 *}
function pixDitherToBinarySpec( pixs: PLPix; lowerclip, upperclip: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *       Simple (pixelwise) binarization with fixed threshold       *
 *------------------------------------------------------------------*}

{*!
 *  pixThresholdToBinary()
 *
 *      Input:  pixs (4 or 8 bpp)
 *              threshold value
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) If the source pixel is less than the threshold value,
 *          the dest will be 1; otherwise, it will be 0
 *}
function pixThresholdToBinary( pixs: PLPix; thresh: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                Binarization with variable threshold              *
 *------------------------------------------------------------------*}

{*!
 *  pixVarThresholdToBinary()
 *
 *      Input:  pixs (8 bpp)
 *              pixg (8 bpp; contains threshold values for each pixel)
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) If the pixel in pixs is less than the corresponding pixel
 *          in pixg, the dest will be 1; otherwise it will be 0.
 *}
function pixVarThresholdToBinary( pixs, pixg: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *    Slower implementation of binarization by dithering using LUTs   *
 *--------------------------------------------------------------------*}

{*!
 *  pixDitherToBinaryLUT()
 *
 *      Input:  pixs
 *              lowerclip (lower clip distance to black; use -1 for default)
 *              upperclip (upper clip distance to white; use -1 for default)
 *      Return: pixd (dithered binary), or null on error
 *
 *  This implementation is deprecated.  You should use pixDitherToBinary().
 *
 *  See comments in pixDitherToBinary()
 *
 *  This implementation additionally uses three lookup tables to
 *  generate the output pixel value and the excess or deficit
 *  carried over to the neighboring pixels.
 *}
function pixDitherToBinaryLUT( pixs: PLPix; lowerclip, upperclip: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *       Generate a binary mask from pixels of particular value(s)    *
 *--------------------------------------------------------------------*}

{*!
 *  pixGenerateMaskByValue()
 *
 *      Input:  pixs (4 or 8 bpp, or colormapped)
 *              val (of pixels for which we set 1 in dest)
 *              usecmap (1 to retain cmap values; 0 to convert to gray)
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) @val is the gray value of the pixels that we are selecting.
 *      (2) If pixs is colormapped, @usecmap determines if the colormap
 *          values are used, or if the colormap is removed to gray and
 *          the gray values are used.  For the latter, it generates
 *          an approximate grayscale value for each pixel, and then looks
 *          for gray pixels with the value @val.
 *}
function pixGenerateMaskByValue( pixs: PLPix; val, usecmap: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixGenerateMaskByBand()
 *
 *      Input:  pixs (4 or 8 bpp, or colormapped)
 *              lower, upper (two pixel values from which a range, either
 *                            between (inband) or outside of (!inband),
 *                            determines which pixels in pixs cause us to
 *                            set a 1 in the dest mask)
 *              inband (1 for finding pixels in [lower, upper];
 *                      0 for finding pixels in [0, lower) union (upper, 255])
 *              usecmap (1 to retain cmap values; 0 to convert to gray)
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) Generates a 1 bpp mask pixd, the same size as pixs, where
 *          the fg pixels in the mask are those either within the specified
 *          band (for inband == 1) or outside the specified band
 *          (for inband == 0).
 *      (2) If pixs is colormapped, @usecmap determines if the colormap
 *          values are used, or if the colormap is removed to gray and
 *          the gray values are used.  For the latter, it generates
 *          an approximate grayscale value for each pixel, and then looks
 *          for gray pixels with the value @val.
 *}
function pixGenerateMaskByBand( pixs: PLPix; lower, upper, inband, usecmap: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                Thresholding to 2 bpp by dithering                *
 *------------------------------------------------------------------*}

{*!
 *  pixDitherTo2bpp()
 *
 *      Input:  pixs (8 bpp)
 *              cmapflag (1 to generate a colormap)
 *      Return: pixd (dithered 2 bpp), or null on error
 *
 *  An analog of the Floyd-Steinberg error diffusion dithering
 *  algorithm is used to "dibitize" an 8 bpp grayscale image
 *  to 2 bpp, using equally spaced gray values of 0, 85, 170, and 255,
 *  which are served by thresholds of 43, 128 and 213.
 *  If cmapflag == 1, the colormap values are set to 0, 85, 170 and 255.
 *  If a pixel has a value between 0 and 42, it is dibitized
 *  to 0, and the excess (above 0) is added to the
 *  three neighboring pixels, in the fractions 3/8 to (i, j+1),
 *  3/8 to (i+1, j) and 1/4 to (i+1, j+1), truncating to 255 if
 *  necessary.  If a pixel has a value between 43 and 127, it is
 *  dibitized to 1, and the excess (above 85) is added to the three
 *  neighboring pixels as before.  If the value is below 85, the
 *  excess is subtracted.  With a value between 128
 *  and 212, it is dibitized to 2, with the excess on either side
 *  of 170 distributed as before.  Finally, with a value between
 *  213 and 255, it is dibitized to 3, with the excess (below 255)
 *  subtracted from the neighbors.  We always truncate to 0 or 255.
 *  The details can be seen in the lookup table generation.
 *
 *  This function differs from straight dithering in that it allows
 *  clipping of grayscale to 0 or 255 if the values are
 *  sufficiently close, without distribution of the excess.
 *  This uses default values (from pix.h) to specify the range of lower
 *  and upper values (near 0 and 255, rsp) that are clipped to black
 *  and white without propagating the excess.
 *  Not propagating the excess has the effect of reducing the snake
 *  patterns in parts of the image that are nearly black or white;
 *  however, it also prevents any attempt to reproduce gray for those values.
 *
 *  The implementation uses 3 lookup tables for simplicity, and
 *  a pair of line buffers to avoid modifying pixs.
 *}
function pixDitherTo2bpp( pixs: PLPix; cmapflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixDitherTo2bppSpec()
 *
 *      Input:  pixs (8 bpp)
 *              lowerclip (lower clip distance to black; use 0 for default)
 *              upperclip (upper clip distance to white; use 0 for default)
 *              cmapflag (1 to generate a colormap)
 *      Return: pixd (dithered 2 bpp), or null on error
 *
 *  Notes:
 *      (1) See comments above in pixDitherTo2bpp() for details.
 *      (2) The input parameters lowerclip and upperclip specify the range
 *          of lower and upper values (near 0 and 255, rsp) that are
 *          clipped to black and white without propagating the excess.
 *          For that reason, lowerclip and upperclip should be small numbers.
 *}
function pixDitherTo2bppSpec( pixs: PLPix; lowerclip, upperclip, cmapflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *  Simple (pixelwise) thresholding to 2 bpp with optional colormap   *
 *--------------------------------------------------------------------*}

{*!
 *  pixThresholdTo2bpp()
 *
 *      Input:  pixs (8 bpp)
 *              nlevels (equally spaced; must be between 2 and 4)
 *              cmapflag (1 to build colormap; 0 otherwise)
 *      Return: pixd (2 bpp, optionally with colormap), or null on error
 *
 *  Notes:
 *      (1) Valid values for nlevels is the set {2, 3, 4}.
 *      (2) Any colormap on the input pixs is removed to 8 bpp grayscale.
 *      (3) This function is typically invoked with cmapflag == 1.
 *          In the situation where no colormap is desired, nlevels is
 *          ignored and pixs is thresholded to 4 levels.
 *      (4) The target output colors are equally spaced, with the
 *          darkest at 0 and the lightest at 255.  The thresholds are
 *          chosen halfway between adjacent output values.  A table
 *          is built that specifies the mapping from src to dest.
 *      (5) If cmapflag == 1, a colormap of size 'nlevels' is made,
 *          and the pixel values in pixs are replaced by their
 *          appropriate color indices.  The number of holdouts,
 *          4 - nlevels, will be between 0 and 2.
 *      (6) If you don't want the thresholding to be equally spaced,
 *          either first transform the 8 bpp src using pixGammaTRC().
 *          or, if cmapflag == 1, after calling this function you can use
 *          pixcmapResetColor() to change any individual colors.
 *      (7) If a colormap is generated, it will specify (to display
 *          programs) exactly how each level is to be represented in RGB
 *          space.  When representing text, 3 levels is far better than
 *          2 because of the antialiasing of the single gray level,
 *          and 4 levels (black, white and 2 gray levels) is getting
 *          close to the perceptual quality of a (nearly continuous)
 *          grayscale image.  With 2 bpp, you can set up a colormap
 *          and allocate from 2 to 4 levels to represent antialiased text.
 *          Any left over colormap entries can be used for coloring regions.
 *          For the same number of levels, the file size of a 2 bpp image
 *          is about 10% smaller than that of a 4 bpp result for the same
 *          number of levels.  For both 2 bpp and 4 bpp, using 4 levels you
 *          get compression far better than that of jpeg, because the
 *          quantization to 4 levels will remove the jpeg ringing in the
 *          background near character edges.
 *}
function pixThresholdTo2bpp( pixs: PLPix; nlevels, cmapflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *               Simple (pixelwise) thresholding to 4 bpp               *
 *----------------------------------------------------------------------*}

{*!
 *  pixThresholdTo4bpp()
 *
 *      Input:  pixs (8 bpp, can have colormap)
 *              nlevels (equally spaced; must be between 2 and 16)
 *              cmapflag (1 to build colormap; 0 otherwise)
 *      Return: pixd (4 bpp, optionally with colormap), or null on error
 *
 *  Notes:
 *      (1) Valid values for nlevels is the set {2, ... 16}.
 *      (2) Any colormap on the input pixs is removed to 8 bpp grayscale.
 *      (3) This function is typically invoked with cmapflag == 1.
 *          In the situation where no colormap is desired, nlevels is
 *          ignored and pixs is thresholded to 16 levels.
 *      (4) The target output colors are equally spaced, with the
 *          darkest at 0 and the lightest at 255.  The thresholds are
 *          chosen halfway between adjacent output values.  A table
 *          is built that specifies the mapping from src to dest.
 *      (5) If cmapflag == 1, a colormap of size 'nlevels' is made,
 *          and the pixel values in pixs are replaced by their
 *          appropriate color indices.  The number of holdouts,
 *          16 - nlevels, will be between 0 and 14.
 *      (6) If you don't want the thresholding to be equally spaced,
 *          either first transform the 8 bpp src using pixGammaTRC().
 *          or, if cmapflag == 1, after calling this function you can use
 *          pixcmapResetColor() to change any individual colors.
 *      (7) If a colormap is generated, it will specify, to display
 *          programs, exactly how each level is to be represented in RGB
 *          space.  When representing text, 3 levels is far better than
 *          2 because of the antialiasing of the single gray level,
 *          and 4 levels (black, white and 2 gray levels) is getting
 *          close to the perceptual quality of a (nearly continuous)
 *          grayscale image.  Therefore, with 4 bpp, you can set up a
 *          colormap, allocate a relatively small fraction of the 16
 *          possible values to represent antialiased text, and use the
 *          other colormap entries for other things, such as coloring
 *          text or background.  Two other reasons for using a small number
 *          of gray values for antialiased text are (1) PNG compression
 *          gets worse as the number of levels that are used is increased,
 *          and (2) using a small number of levels will filter out most of
 *          the jpeg ringing that is typically introduced near sharp edges
 *          of text.  This filtering is partly responsible for the improved
 *          compression.
 *}
function pixThresholdTo4bpp( pixs: PLPix; nlevels, cmapflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *    Simple (pixelwise) thresholding on 8 bpp with optional colormap   *
 *----------------------------------------------------------------------*}

{*!
 *  pixThresholdOn8bpp()
 *
 *      Input:  pixs (8 bpp, can have colormap)
 *              nlevels (equally spaced; must be between 2 and 256)
 *              cmapflag (1 to build colormap; 0 otherwise)
 *      Return: pixd (8 bpp, optionally with colormap), or null on error
 *
 *  Notes:
 *      (1) Valid values for nlevels is the set {2,...,256}.
 *      (2) Any colormap on the input pixs is removed to 8 bpp grayscale.
 *      (3) If cmapflag == 1, a colormap of size 'nlevels' is made,
 *          and the pixel values in pixs are replaced by their
 *          appropriate color indices.  Otherwise, the pixel values
 *          are the actual thresholded (i.e., quantized) grayscale values.
 *      (4) If you don't want the thresholding to be equally spaced,
 *          first transform the input 8 bpp src using pixGammaTRC().
 *}
function pixThresholdOn8bpp( pixs: PLPix; nlevels, cmapflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *    Arbitrary (pixelwise) thresholding from 8 bpp to 2, 4 or 8 bpp    *
 *----------------------------------------------------------------------*}

{*!
 *  pixThresholdGrayArb()
 *
 *      Input:  pixs (8 bpp grayscale; can have colormap)
 *              edgevals (string giving edge value of each bin)
 *              outdepth (0, 2, 4 or 8 bpp; 0 is default for min depth)
 *              use_average (1 if use the average pixel value in colormap)
 *              setblack (1 if darkest color is set to black)
 *              setwhite (1 if lightest color is set to white)
 *      Return: pixd (2, 4 or 8 bpp quantized image with colormap),
 *                    or null on error
 *
 *  Notes:
 *      (1) This function allows exact specification of the quantization bins.
 *          The string @edgevals is a space-separated set of values
 *          specifying the dividing points between output quantization bins.
 *          These threshold values are assigned to the bin with higher
 *          values, so that each of them is the smallest value in their bin.
 *      (2) The output image (pixd) depth is specified by @outdepth.  The
 *          number of bins is the number of edgevals + 1.  The
 *          relation between outdepth and the number of bins is:
 *               outdepth = 2       nbins <= 4
 *               outdepth = 4       nbins <= 16
 *               outdepth = 8       nbins <= 256
 *          With @outdepth == 0, the minimum required depth for the
 *          given number of bins is used.
 *          The output pixd has a colormap.
 *      (3) The last 3 args determine the specific values that go into
 *          the colormap.
 *      (4) For @use_average:
 *            - if TRUE, the average value of pixels falling in the bin is
 *              chosen as the representative gray value.  Otherwise,
 *            - if FALSE, the central value of each bin is chosen as
 *              the representative value.
 *          The colormap holds the representative value.
 *      (5) For @setblack, if TRUE the darkest color is set to (0,0,0).
 *      (6) For @setwhite, if TRUE the lightest color is set to (255,255,255).
 *      (7) An alternative to using this function to quantize to
 *          unequally-spaced bins is to first transform the 8 bpp pixs
 *          using pixGammaTRC(), and follow this with pixThresholdTo4bpp().
 *}
function pixThresholdGrayArb( pixs: PLPix; edgevals: PChar; outdepth, use_average, setblack, setwhite: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *     Quantization tables for linear thresholds of grayscale images    *
 *----------------------------------------------------------------------*}

{*!
 *  makeGrayQuantIndexTable()
 *
 *      Input:  nlevels (number of output levels)
 *      Return: table (maps input gray level to colormap index,
 *                     or null on error)
 *  Notes:
 *      (1) 'nlevels' is some number between 2 and 256 (typically 8 or less).
 *      (2) The table is typically used for quantizing 2, 4 and 8 bpp
 *          grayscale src pix, and generating a colormapped dest pix.
 *}
function makeGrayQuantIndexTable( nlevels: LongInt ) : PLongint; cdecl; external LIBLEPT;


{*!
 *  makeGrayQuantTargetTable()
 *
 *      Input:  nlevels (number of output levels)
 *              depth (of dest pix, in bpp; 2, 4 or 8 bpp)
 *      Return: table (maps input gray level to thresholded gray level,
 *                     or null on error)
 *
 *  Notes:
 *      (1) nlevels is some number between 2 and 2^(depth)
 *      (2) The table is used in two similar ways:
 *           - for 8 bpp, it quantizes to a given number of target levels
 *           - for 2 and 4 bpp, it thresholds to appropriate target values
 *             that will use the full dynamic range of the dest pix.
 *      (3) For depth = 8, the number of thresholds chosen is
 *          ('nlevels' - 1), and the 'nlevels' values stored in the
 *          table are at the two at the extreme ends, (0, 255), plus
 *          plus ('nlevels' - 2) values chosen at equal intervals between.
 *          For example, for depth = 8 and 'nlevels' = 3, the two
 *          threshold values are 3f and bf, and the three target pixel
 *          values are 0, 7f and ff.
 *      (4) For depth < 8, we ignore nlevels, and always use the maximum
 *          number of levels, which is 2^(depth).
 *          If you want nlevels < the maximum number, you should always
 *          use a colormap.
 *}
function makeGrayQuantTargetTable( nlevels, depth: LongInt ) : PLongint; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *   Quantization table for arbitrary thresholding of grayscale images  *
 *----------------------------------------------------------------------*}

{*!
 *  makeGrayQuantTableArb()
 *
 *      Input:  na (numa of bin boundaries)
 *              outdepth (of colormap: 1, 2, 4 or 8)
 *              &tab (<return> table mapping input gray level to cmap index)
 *              &cmap (<return> colormap)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The number of bins is the count of @na + 1.
 *      (2) The bin boundaries in na must be sorted in increasing order.
 *      (3) The table is an inverse colormap: it maps input gray level
 *          to colormap index (the bin number).
 *      (4) The colormap generated here has quantized values at the
 *          center of each bin.  If you want to use the average gray
 *          value of pixels within the bin, discard the colormap and
 *          compute it using makeGrayQuantColormapArb().
 *      (5) Returns an error if there are not enough levels in the
 *          output colormap for the number of bins.  The number
 *          of bins must not exceed 2^outdepth.
 *}
function makeGrayQuantTableArb( na: PNumArray; outdepth: LongInt; ptab: PPLongInt; pcmap: PPPixCmap ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  makeGrayQuantColormapArb()
 *
 *      Input:  pixs (8 bpp)
 *              tab (table mapping input gray level to cmap index)
 *              outdepth (of colormap: 1, 2, 4 or 8)
 *              &cmap (<return> colormap)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The table is a 256-entry inverse colormap: it maps input gray
 *          level to colormap index (the bin number).  It is computed
 *          using makeGrayQuantTableArb().
 *      (2) The colormap generated here has quantized values at the
 *          average gray value of the pixels that are in each bin.
 *      (3) Returns an error if there are not enough levels in the
 *          output colormap for the number of bins.  The number
 *          of bins must not exceed 2^outdepth.
 *}
function makeGrayQuantColormapArb( pixs: PLPix; tab: PLongint; outdepth: LongInt; pcmap: PPPixCmap ) : LongInt; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *                 Thresholding from 32 bpp rgb to 1 bpp              *
 *--------------------------------------------------------------------*}

{*!
 *  pixGenerateMaskByBand32()
 *
 *      Input:  pixs (32 bpp)
 *              refval (reference rgb value)
 *              delm (max amount below the ref value for any component)
 *              delp (max amount above the ref value for any component)
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) Generates a 1 bpp mask pixd, the same size as pixs, where
 *          the fg pixels in the mask are those where each component
 *          is within -delm to +delp of the reference value.
 *}
function pixGenerateMaskByBand32( pixs: PLPix; refval: LongWord; delm, delp: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixGenerateMaskByDiscr32()
 *
 *      Input:  pixs (32 bpp)
 *              refval1 (reference rgb value)
 *              refval2 (reference rgb value)
 *              distflag (L_MANHATTAN_DISTANCE, L_EUCLIDEAN_DISTANCE)
 *      Return: pixd (1 bpp), or null on error
 *
 *  Notes:
 *      (1) Generates a 1 bpp mask pixd, the same size as pixs, where
 *          the fg pixels in the mask are those where the pixel in pixs
 *          is "closer" to refval1 than to refval2.
 *      (2) "Closer" can be defined in several ways, such as:
 *            - manhattan distance (L1)
 *            - euclidean distance (L2)
 *            - majority vote of the individual components
 *          Here, we have a choice of L1 or L2.
 *}
function pixGenerateMaskByDiscr32( pixs: PLPix; refval1, refval2: LongWord; distflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *                Histogram-based grayscale quantization                *
 *----------------------------------------------------------------------*}

{*!
 *  pixGrayQuantFromHisto()
 *
 *      Input:  pixd (<optional> quantized pix with cmap; can be null)
 *              pixs (8 bpp gray input pix; not cmapped)
 *              pixm (<optional> mask over pixels in pixs to quantize)
 *              minfract (minimum fraction of pixels in a set of adjacent
 *                        histo bins that causes the set to be automatically
 *                        set aside as a color in the colormap; must be
 *                        at least 0.01)
 *              maxsize (maximum number of adjacent bins allowed to represent
 *                       a color, regardless of the population of pixels
 *                       in the bins; must be at least 2)
 *      Return: pixd (8 bpp, cmapped), or null on error
 *
 *  Notes:
 *      (1) This is useful for quantizing images with relatively few
 *          colors, but which may have both color and gray pixels.
 *          If there are color pixels, it is assumed that an input
 *          rgb image has been color quantized first so that:
 *            - pixd has a colormap describing the color pixels
 *            - pixm is a mask over the non-color pixels in pixd
 *            - the colormap in pixd, and the color pixels in pixd,
 *              have been repacked to go from 0 to n-1 (n colors)
 *          If there are no color pixels, pixd and pixm are both null,
 *          and all pixels in pixs are quantized to gray.
 *      (2) A 256-entry histogram is built of the gray values in pixs.
 *          If pixm exists, the pixels contributing to the histogram are
 *          restricted to the fg of pixm.  A colormap and LUT are generated
 *          from this histogram.  We break up the array into a set
 *          of intervals, each one constituting a color in the colormap:
 *          An interval is identified by summing histogram bins until
 *          either the sum equals or exceeds the @minfract of the total
 *          number of pixels, or the span itself equals or exceeds @maxsize.
 *          The color of each bin is always an average of the pixels
 *          that constitute it.
 *      (3) Note that we do not specify the number of gray colors in
 *          the colormap.  Instead, we specify two parameters that
 *          describe the accuracy of the color assignments; this and
 *          the actual image determine the number of resulting colors.
 *      (4) If a mask exists and it is not the same size as pixs, make
 *          a new mask the same size as pixs, with the original mask
 *          aligned at the UL corners.  Set all additional pixels
 *          in the (larger) new mask set to 1, causing those pixels
 *          in pixd to be set as gray.
 *      (5) We estimate the total number of colors (color plus gray);
 *          if it exceeds 255, return null.
 *}
function pixGrayQuantFromHisto( pixd, pixs, pixm: PLPix; minfract: Single; maxsize: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  numaFillCmapFromHisto()
 *
 *      Input:  na (histogram of gray values)
 *              cmap (8 bpp cmap, possibly initialized with color value)
 *              minfract (minimum fraction of pixels in a set of adjacent
 *                        histo bins that causes the set to be automatically
 *                        set aside as a color in the colormap; must be
 *                        at least 0.01)
 *              maxsize (maximum number of adjacent bins allowed to represent
 *                       a color, regardless of the population of pixels
 *                       in the bins; must be at least 2)
 *             &lut (<return> lookup table from gray value to colormap index)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This static function must be called from pixGrayQuantFromHisto()
 *}

{*----------------------------------------------------------------------*
 *        Color quantize grayscale image using existing colormap        *
 *----------------------------------------------------------------------*}

{*!
 *  pixGrayQuantFromCmap()
 *
 *      Input:  pixs (8 bpp grayscale without cmap)
 *              cmap (to quantize to; of dest pix)
 *              mindepth (minimum depth of pixd: can be 2, 4 or 8 bpp)
 *      Return: pixd (2, 4 or 8 bpp, colormapped), or null on error
 *
 *  Notes:
 *      (1) In use, pixs is an 8 bpp grayscale image without a colormap.
 *          If there is an existing colormap, a warning is issued and
 *          a copy of the input pixs is returned.
 *}
function pixGrayQuantFromCmap( pixs: PLPix; cmap: PPixCmap; mindepth: LongInt ) : PLPix; cdecl; external LIBLEPT;


