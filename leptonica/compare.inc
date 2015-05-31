{ This file is an automated translation to pascal of compare.c                }
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
 *  compare.c
 *
 *      Test for pix equality
 *           l_int32     pixEqual()
 *           l_int32     pixEqualWithCmap()
 *           l_int32     pixUsesCmapColor()
 *
 *      Binary correlation
 *           l_int32     pixCorrelationBinary()
 *
 *      Difference of two images of same size
 *           l_int32     pixDisplayDiffBinary()
 *           l_int32     pixCompareBinary()
 *           l_int32     pixCompareGrayOrRGB()
 *           l_int32     pixCompareGray()
 *           l_int32     pixCompareRGB()
 *           l_int32     pixCompareTiled()
 *
 *      Other measures of the difference of two images of the same size
 *           NUMA       *pixCompareRankDifference()
 *           l_int32     pixTestforSimilarity()
 *           l_int32     pixGetDifferenceStats()
 *           NUMA       *pixGetDifferenceHistogram()
 *           l_int32     pixGetPSNR()
 *
 *      Translated images at the same resolution
 *           l_int32     pixCompareWithTranslation()
 *}

{* Small enough to consider equal to 0.0, for plot output *}
const TINY : Single = 0.00001;     

{*------------------------------------------------------------------*
 *                        Test for pix equality                     *
 *------------------------------------------------------------------*}

{*!
 *  pixEqual()
 *
 *      Input:  pix1
 *              pix2
 *              &same  (<return> 1 if same; 0 if different)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Equality is defined as having the same pixel values for
 *          each respective image pixel.
 *      (2) This works on two pix of any depth.  If one or both pix
 *          have a colormap, the depths can be different and the
 *          two pix can still be equal.
 *      (3) If both pix have colormaps and the depths are equal,
 *          use the pixEqualWithCmap() function, which does a fast
 *          comparison if the colormaps are identical and a relatively
 *          slow comparison otherwise.
 *      (4) In all other cases, any existing colormaps must first be
 *          removed before doing pixel comparison.  After the colormaps
 *          are removed, the resulting two images must have the same depth.
 *          The "lowest common denominator" is RGB, but this is only
 *          chosen when necessary, or when both have colormaps but
 *          different depths.
 *      (5) For 32 bpp, ignore the bits in the 4th byte (the 'A' byte
 *          of the RGBA pixel)
 *      (6) For images without colormaps that are not 32 bpp, all bits
 *          in the image part of the data array must be identical.
 *}
function pixEqual( pix1, pix2: PLPix; psame: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixEqualWithCmap()
 *
 *      Input:  pix1
 *              pix2
 *              &same
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This returns same = TRUE if the images have identical content.
 *      (2) Both pix must have a colormap, and be of equal size and depth.
 *          If these conditions are not satisfied, it is not an error;
 *          the returned result is same = FALSE.
 *      (3) We then check whether the colormaps are the same; if so,
 *          the comparison proceeds 32 bits at a time.
 *      (4) If the colormaps are different, the comparison is done by
 *          slow brute force.
 *}
function pixEqualWithCmap( pix1, pix2: PLPix; psame: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixUsesCmapColor()
 *
 *      Input:  pixs
 *              &color (<return>)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This returns color = TRUE if three things are obtained:
 *          (a) the pix has a colormap
 *          (b) the colormap has at least one color entry
 *          (c) a color entry is actually used
 *      (2) It is used in pixEqual() for comparing two images, in a
 *          situation where it is required to know if the colormap
 *          has color entries that are actually used in the image.
 *}
function pixUsesCmapColor( pixs: PLPix; pcolor: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                          Binary correlation                      *
 *------------------------------------------------------------------*}

{*!
 *  pixCorrelationBinary()
 *
 *      Input:  pix1 (1 bpp)
 *              pix2 (1 bpp)
 *              &val (<return> correlation)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The correlation is a number between 0.0 and 1.0,
 *          based on foreground similarity:
 *                           (|1 AND 2|)**2
 *            correlation =  --------------
 *                             |1| * |2|
 *          where |x| is the count of foreground pixels in image x.
 *          If the images are identical, this is 1.0.
 *          If they have no fg pixels in common, this is 0.0.
 *          If one or both images have no fg pixels, the correlation is 0.0.
 *      (2) Typically the two images are of equal size, but this
 *          is not enforced.  Instead, the UL corners are be aligned.
 *}
function pixCorrelationBinary( pix1, pix2: PLPix; pval: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                   Difference of two images                       *
 *------------------------------------------------------------------*}

{*!
 *  pixDisplayDiffBinary()
 *
 *      Input:  pix1 (1 bpp)
 *              pix2 (1 bpp)
 *      Return: pixd (4 bpp cmapped), or null on error
 *
 *  Notes:
 *      (1) This gives a color representation of the difference between
 *          pix1 and pix2.  The color difference depends on the order.
 *          The pixels in pixd have 4 colors:
 *           * unchanged:  black (on), white (off)
 *           * on in pix1, off in pix2: red
 *           * on in pix2, off in pix1: green
 *      (2) This aligns the UL corners of pix1 and pix2, and crops
 *          to the overlapping pixels.
 *}
function pixDisplayDiffBinary( pix1, pix2: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCompareBinary()
 *
 *      Input:  pix1 (1 bpp)
 *              pix2 (1 bpp)
 *              comptype (L_COMPARE_XOR, L_COMPARE_SUBTRACT)
 *              &fract (<return> fraction of pixels that are different)
 *              &pixdiff (<optional return> pix of difference)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The two images are aligned at the UL corner, and do not
 *          need to be the same size.
 *      (2) If using L_COMPARE_SUBTRACT, pix2 is subtracted from pix1.
 *      (3) The total number of pixels is determined by pix1.
 *}
function pixCompareBinary( pix1, pix2: PLPix; comptype: LongInt; pfract: PSingle; ppixdiff: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCompareGrayOrRGB()
 *
 *      Input:  pix1 (8 or 16 bpp gray, 32 bpp rgb, or colormapped)
 *              pix2 (8 or 16 bpp gray, 32 bpp rgb, or colormapped)
 *              comptype (L_COMPARE_SUBTRACT, L_COMPARE_ABS_DIFF)
 *              plottype (gplot plot output type, or 0 for no plot)
 *              &same (<optional return> 1 if pixel values are identical)
 *              &diff (<optional return> average difference)
 *              &rmsdiff (<optional return> rms of difference)
 *              &pixdiff (<optional return> pix of difference)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) The two images are aligned at the UL corner, and do not
 *          need to be the same size.  If they are not the same size,
 *          the comparison will be made over overlapping pixels.
 *      (2) If there is a colormap, it is removed and the result
 *          is either gray or RGB depending on the colormap.
 *      (3) If RGB, each component is compared separately.
 *      (4) If type is L_COMPARE_ABS_DIFF, pix2 is subtracted from pix1
 *          and the absolute value is taken.
 *      (5) If type is L_COMPARE_SUBTRACT, pix2 is subtracted from pix1
 *          and the result is clipped to 0.
 *      (6) The plot output types are specified in gplot.h.
 *          Use 0 if no difference plot is to be made.
 *      (7) If the images are pixelwise identical, no difference
 *          plot is made, even if requested.  The result (TRUE or FALSE)
 *          is optionally returned in the parameter 'same'.
 *      (8) The average difference (either subtracting or absolute value)
 *          is optionally returned in the parameter 'diff'.
 *      (9) The RMS difference is optionally returned in the
 *          parameter 'rmsdiff'.  For RGB, we return the average of
 *          the RMS differences for each of the components.
 *}
function pixCompareGrayOrRGB( pix1, pix2: PLPix; comptype, plottype: LongInt; psame: PLongint; pdiff, prmsdiff: PSingle; ppixdiff: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCompareGray()
 *
 *      Input:  pix1 (8 or 16 bpp, not cmapped)
 *              pix2 (8 or 16 bpp, not cmapped)
 *              comptype (L_COMPARE_SUBTRACT, L_COMPARE_ABS_DIFF)
 *              plottype (gplot plot output type, or 0 for no plot)
 *              &same (<optional return> 1 if pixel values are identical)
 *              &diff (<optional return> average difference)
 *              &rmsdiff (<optional return> rms of difference)
 *              &pixdiff (<optional return> pix of difference)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) See pixCompareGrayOrRGB() for details.
 *      (2) Use pixCompareGrayOrRGB() if the input pix are colormapped.
 *}
function pixCompareGray( pix1, pix2: PLPix; comptype, plottype: LongInt; psame: PLongint; pdiff, prmsdiff: PSingle; ppixdiff: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCompareRGB()
 *
 *      Input:  pix1 (32 bpp rgb)
 *              pix2 (32 bpp rgb)
 *              comptype (L_COMPARE_SUBTRACT, L_COMPARE_ABS_DIFF)
 *              plottype (gplot plot output type, or 0 for no plot)
 *              &same (<optional return> 1 if pixel values are identical)
 *              &diff (<optional return> average difference)
 *              &rmsdiff (<optional return> rms of difference)
 *              &pixdiff (<optional return> pix of difference)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) See pixCompareGrayOrRGB() for details.
 *}
function pixCompareRGB( pix1, pix2: PLPix; comptype, plottype: LongInt; psame: PLongint; pdiff, prmsdiff: PSingle; ppixdiff: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCompareTiled()
 *
 *      Input:  pix1 (8 bpp or 32 bpp rgb)
 *              pix2 (8 bpp 32 bpp rgb)
 *              sx, sy (tile size; must be > 1)
 *              type (L_MEAN_ABSVAL or L_ROOT_MEAN_SQUARE)
 *              &pixdiff (<return> pix of difference)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) With L_MEAN_ABSVAL, we compute for each tile the
 *          average abs value of the pixel component difference between
 *          the two (aligned) images.  With L_ROOT_MEAN_SQUARE, we
 *          compute instead the rms difference over all components.
 *      (2) The two input pix must be the same depth.  Comparison is made
 *          using UL corner alignment.
 *      (3) For 32 bpp, the distance between corresponding tiles
 *          is found by averaging the measured difference over all three
 *          components of each pixel in the tile.
 *      (4) The result, pixdiff, contains one pixel for each source tile.
 *}
function pixCompareTiled( pix1, pix2: PLPix; sx, sy, L_type: LongInt; ppixdiff: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *            Other measures of the difference of two images        *
 *------------------------------------------------------------------*}

{*!
 *  pixCompareRankDifference()
 *
 *      Input:  pix1 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              pix2 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              factor (subsampling factor; use 0 or 1 for no subsampling)
 *      Return: narank (numa of rank difference), or null on error
 *
 *  Notes:
 *      (1) This answers the question: if the pixel values in each
 *          component are compared by absolute difference, for
 *          any value of difference, what is the fraction of
 *          pixel pairs that have a difference of this magnitude
 *          or greater.  For a difference of 0, the fraction is 1.0.
 *          In this sense, it is a mapping from pixel difference to
 *          rank order of difference.
 *      (2) The two images are aligned at the UL corner, and do not
 *          need to be the same size.  If they are not the same size,
 *          the comparison will be made over overlapping pixels.
 *      (3) If there is a colormap, it is removed and the result
 *          is either gray or RGB depending on the colormap.
 *      (4) If RGB, pixel differences for each component are aggregated
 *          into a single histogram.
 *}
function pixCompareRankDifference( pix1, pix2: PLPix; factor: LongInt ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixTestForSimilarity()
 *
 *      Input:  pix1 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              pix2 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              factor (subsampling factor; use 0 or 1 for no subsampling)
 *              mindiff (minimum pixel difference to be counted; > 0)
 *              maxfract (maximum fraction of pixels allowed to have
 *                        diff greater than or equal to mindiff)
 *              maxave (maximum average difference of pixels allowed for
 *                      pixels with diff greater than or equal to mindiff,
 *                      after subtracting mindiff)
 *              &similar (<return> 1 if similar, 0 otherwise)
 *              printstats (use 1 to print normalized histogram to stderr)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This takes 2 pix that are the same size and determines using
 *          3 input parameters if they are "similar".  The first parameter
 *          @mindiff establishes a criterion of pixel-to-pixel similarity:
 *          two pixels are not similar if their difference in value is
 *          at least mindiff.  Then @maxfract and @maxave are thresholds
 *          on the number and distribution of dissimilar pixels
 *          allowed for the two pix to be similar.   If the pix are
 *          to be similar, neither threshold can be exceeded.
 *      (2) In setting the @maxfract and @maxave thresholds, you have
 *          these options:
 *            (a) Base the comparison only on @maxfract.  Then set
 *                @maxave = 0.0 or 256.0.  (If 0, we always ignore it.)
 *            (b) Base the comparison only on @maxave.  Then set
 *                @maxfract = 1.0.
 *            (c) Base the comparison on both thresholds.
 *      (3) Example of values that can be expected at mindiff = 15 when
 *          comparing lossless png encoding with jpeg encoding, q=75:
 *             (smoothish bg)       fractdiff = 0.01, avediff = 2.5
 *             (natural scene)      fractdiff = 0.13, avediff = 3.5
 *          To identify these images as 'similar', select maxfract
 *          and maxave to be upper bounds of what you expect.
 *      (4) See pixGetDifferenceStats() for a discussion of why we subtract
 *          mindiff from the computed average diff of the nonsimilar pixels
 *          to get the 'avediff' returned by that function.
 *      (5) If there is a colormap, it is removed and the result
 *          is either gray or RGB depending on the colormap.
 *      (6) If RGB, the maximum difference between pixel components is
 *          saved in the histogram.
 *}
function pixTestForSimilarity( pix1, pix2: PLPix; factor, mindiff: LongInt; maxfract, maxave: Single; psimilar: PLongint; printstats: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetDifferenceStats()
 *
 *      Input:  pix1 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              pix2 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              factor (subsampling factor; use 0 or 1 for no subsampling)
 *              mindiff (minimum pixel difference to be counted; > 0)
 *              &fractdiff (<return> fraction of pixels with diff greater
 *                          than or equal to mindiff)
 *              &avediff (<return> average difference of pixels with diff
 *                        greater than or equal to mindiff, less mindiff)
 *              printstats (use 1 to print normalized histogram to stderr)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This takes a threshold @mindiff and describes the difference
 *          between two images in terms of two numbers:
 *            (a) the fraction of pixels, @fractdiff, whose difference
 *                equals or exceeds the threshold @mindiff, and
 *            (b) the average value @avediff of the difference in pixel value
 *                for the pixels in the set given by (a), after you subtract
 *                @mindiff.  The reason for subtracting @mindiff is that
 *                you then get a useful measure for the rate of falloff
 *                of the distribution for larger differences.  For example,
 *                if @mindiff = 10 and you find that @avediff = 2.5, it
 *                says that of the pixels with diff > 10, the average of
 *                their diffs is just mindiff + 2.5 = 12.5.  This is a
 *                fast falloff in the histogram with increasing difference.
 *      (2) The two images are aligned at the UL corner, and do not
 *          need to be the same size.  If they are not the same size,
 *          the comparison will be made over overlapping pixels.
 *      (3) If there is a colormap, it is removed and the result
 *          is either gray or RGB depending on the colormap.
 *      (4) If RGB, the maximum difference between pixel components is
 *          saved in the histogram.
 *}
function pixGetDifferenceStats( pix1, pix2: PLPix; factor, mindiff: LongInt; pfractdiff, pavediff: PSingle; printstats: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetDifferenceHistogram()
 *
 *      Input:  pix1 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              pix2 (8 bpp gray or 32 bpp rgb, or colormapped)
 *              factor (subsampling factor; use 0 or 1 for no subsampling)
 *      Return: na (Numa of histogram of differences), or null on error
 *
 *  Notes:
 *      (1) The two images are aligned at the UL corner, and do not
 *          need to be the same size.  If they are not the same size,
 *          the comparison will be made over overlapping pixels.
 *      (2) If there is a colormap, it is removed and the result
 *          is either gray or RGB depending on the colormap.
 *      (3) If RGB, the maximum difference between pixel components is
 *          saved in the histogram.
 *}
function pixGetDifferenceHistogram( pix1, pix2: PLPix; factor: LongInt ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixGetPSNR()
 *
 *      Input:  pix1, pix2 (8 or 32 bpp; no colormap)
 *              factor (sampling factor; >= 1)
 *              &psnr (<return> power signal/noise ratio difference)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This computes the power S/N ratio, in dB, for the difference
 *          between two images.  By convention, the power S/N
 *          for a grayscale image is ('log' == log base 10,
 *          and 'ln == log base e):
 *            PSNR = 10 * log((255/MSE)^2)
 *                 = 4.3429 * ln((255/MSE)^2)
 *                 = -4.3429 * ln((MSE/255)^2)
 *          where MSE is the mean squared error.
 *}
function pixGetPSNR( pix1, pix2: PLPix; factor: LongInt; ppsnr: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *             Translated images at the same resolution             *
 *------------------------------------------------------------------*}

{*!
 *  pixCompareWithTranslation()
 *
 *      Input:  pix1, pix2 (any depth; colormap OK)
 *              thresh (threshold for converting to 1 bpp)
 *              &delx (<return> x translation on pix2 to align with pix1)
 *              &dely (<return> y translation on pix2 to align with pix1)
 *              &score (<return> correlation score at best alignment)
 *              debugflag (1 for debug output; 0 for no debugging)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This does a coarse-to-fine search for best translational
 *          alignment of two images, measured by a scoring function
 *          that is the correlation between the fg pixels.
 *      (2) The threshold is used if the images aren't 1 bpp.
 *      (3) With debug on, you get a pdf that shows, as a grayscale
 *          image, the score as a function of shift from the initial
 *          estimate, for each of the four levels.  The shift is 0 at
 *          the center of the image.
 *      (4) With debug on, you also get a pdf that shows the
 *          difference at the best alignment between the two images,
 *          at each of the four levels.  The red and green pixels
 *          show locations where one image has a fg pixel and the
 *          other doesn't.  The black pixels are where both images
 *          have fg pixels, and white pixels are where neither image
 *          has fg pixels.
 *}
function pixCompareWithTranslation( pix1, pix2: PLPix; thresh: LongInt; pdelx, pdely: PLongint; pscore: PSingle; debugflag: LongInt ) : LongInt; cdecl; external LIBLEPT;


