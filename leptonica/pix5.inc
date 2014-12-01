{ This file is an automated translation to pascal of pix5.c                   }
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
 *  pix5.c
 *
 *    This file has these operations:
 *
 *      (1) Measurement of 1 bpp image properties
 *      (2) Extract rectangular region
 *      (3) Clip to foreground
 *      (4) Extract pixel averages and reversals along lines
 *      (5) Rank row and column transforms
 *
 *    Measurement of properties
 *           l_int32     pixaFindDimensions()
 *           NUMA       *pixaFindAreaPerimRatio()
 *           l_int32     pixFindAreaPerimRatio()
 *           NUMA       *pixaFindPerimSizeRatio()
 *           l_int32     pixFindPerimSizeRatio()
 *           NUMA       *pixaFindAreaFraction()
 *           l_int32     pixFindAreaFraction()
 *           NUMA       *pixaFindWidthHeightRatio()
 *           NUMA       *pixaFindWidthHeightProduct()
 *           l_int32     pixFindOverlapFraction()
 *           BOXA       *pixFindRectangleComps()
 *           l_int32     pixConformsToRectangle()
 *
 *    Extract rectangular region
 *           PIX        *pixClipRectangle()
 *           PIX        *pixClipMasked()
 *           PIX        *pixResizeToMatch()
 *
 *    Clip to foreground
 *           PIX        *pixClipToForeground()
 *           l_int32     pixClipBoxToForeground()
 *           l_int32     pixScanForForeground()
 *           l_int32     pixClipBoxToEdges()
 *           l_int32     pixScanForEdge()
 *
 *    Extract pixel averages and reversals along lines
 *           NUMA       *pixExtractOnLine()
 *           l_float32   pixAverageOnLine();
 *           NUMA       *pixAverageIntensityProfile()
 *           NUMA       *pixReversalProfile()
 *
 *    Rank row and column transforms
 *           PIX        *pixRankRowTransform()
 *           PIX        *pixRankColumnTransform()
 *}

{* ~NO_CONSOLE_IO *}

{*-------------------------------------------------------------*
 *                 Measurement of properties                   *
 *-------------------------------------------------------------*}

{*!
 *  pixaFindDimensions()
 *
 *      Input:  pixa
 *              &naw (<optional return> numa of pix widths)
 *              &nah (<optional return> numa of pix heights)
 *      Return: 0 if OK, 1 on error
 *}
function pixaFindDimensions( pixa: PPixArray; pnaw, pnah: PPNumArray ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixaFindAreaPerimRatio()
 *
 *      Input:  pixa (of 1 bpp pix)
 *      Return: na (of area/perimeter ratio for each pix), or null on error
 *
 *  Notes:
 *      (1) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixaFindAreaPerimRatio( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixFindAreaPerimRatio()
 *
 *      Input:  pixs (1 bpp)
 *              tab (<optional> pixel sum table, can be NULL)
 *              &fract (<return> area/perimeter ratio)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The area is the number of fg pixels that are not on the
 *          boundary (i.e., not 8-connected to a bg pixel), and the
 *          perimeter is the number of boundary fg pixels.
 *      (2) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixFindAreaPerimRatio( pixs: PLPix; tab: PLongint; pfract: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixaFindPerimSizeRatio()
 *
 *      Input:  pixa (of 1 bpp pix)
 *      Return: na (of fg perimeter/(w*h) ratio for each pix), or null on error
 *
 *  Notes:
 *      (1) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixaFindPerimSizeRatio( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixFindPerimSizeRatio()
 *
 *      Input:  pixs (1 bpp)
 *              tab (<optional> pixel sum table, can be NULL)
 *              &ratio (<return> perimeter/size ratio)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The size is the sum of the width and height of the pix,
 *          and the perimeter is the number of boundary fg pixels.
 *      (2) This has a large value for dendritic, fractal-like components
 *          with highly irregular boundaries.
 *      (3) This is typically used for a single connected component.
 *}
function pixFindPerimSizeRatio( pixs: PLPix; tab: PLongint; pratio: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixaFindAreaFraction()
 *
 *      Input:  pixa (of 1 bpp pix)
 *      Return: na (of area fractions for each pix), or null on error
 *
 *  Notes:
 *      (1) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixaFindAreaFraction( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixFindAreaFraction()
 *
 *      Input:  pixs (1 bpp)
 *              tab (<optional> pixel sum table, can be NULL)
 *              &fract (<return> fg area/size ratio)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This finds the ratio of the number of fg pixels to the
 *          size of the pix (w * h).  It is typically used for a
 *          single connected component.
 *}
function pixFindAreaFraction( pixs: PLPix; tab: PLongint; pfract: PSingle ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixaFindWidthHeightRatio()
 *
 *      Input:  pixa (of 1 bpp pix)
 *      Return: na (of width/height ratios for each pix), or null on error
 *
 *  Notes:
 *      (1) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixaFindWidthHeightRatio( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixaFindWidthHeightProduct()
 *
 *      Input:  pixa (of 1 bpp pix)
 *      Return: na (of width*height products for each pix), or null on error
 *
 *  Notes:
 *      (1) This is typically used for a pixa consisting of
 *          1 bpp connected components.
 *}
function pixaFindWidthHeightProduct( pixa: PPixArray ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixFindOverlapFraction()
 *
 *      Input:  pixs1, pixs2 (1 bpp)
 *              x2, y2 (location in pixs1 of UL corner of pixs2)
 *              tab (<optional> pixel sum table, can be null)
 *              &ratio (<return> ratio fg intersection to fg union)
 *              &noverlap (<optional return> number of overlapping pixels)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The UL corner of pixs2 is placed at (x2, y2) in pixs1.
 *      (2) This measure is similar to the correlation.
 *}
function pixFindOverlapFraction( pixs1, pixs2: PLPix; x2, y2: LongInt; tab: PLongint; pratio: PSingle; pnoverlap: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixFindRectangleComps()
 *
 *      Input:  pixs (1 bpp)
 *              dist (max distance allowed between bounding box and nearest
 *                    foreground pixel within it)
 *              minw, minh (minimum size in each direction as a requirement
 *                          for a conforming rectangle)
 *      Return: boxa (of components that conform), or null on error
 *
 *  Notes:
 *      (1) This applies the function pixConformsToRectangle() to
 *          each 8-c.c. in pixs, and returns a boxa containing the
 *          regions of all components that are conforming.
 *      (2) Conforming components must satisfy both the size constraint
 *          given by @minsize and the slop in conforming to a rectangle
 *          determined by @dist.
 *}
function pixFindRectangleComps( pixs: PLPix; dist, minw, minh: LongInt ) : PBoxArray; cdecl; external LIBLEPT;


{*!
 *  pixConformsToRectangle()
 *
 *      Input:  pixs (1 bpp)
 *              box (<optional> if null, use the entire pixs)
 *              dist (max distance allowed between bounding box and nearest
 *                    foreground pixel within it)
 *              &conforms (<return> 0 (false) if not conforming;
 *                        1 (true) if conforming)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) There are several ways to test if a connected component has
 *          an essentially rectangular boundary, such as:
 *           a. Fraction of fill into the bounding box
 *           b. Max-min distance of fg pixel from periphery of bounding box
 *           c. Max depth of bg intrusions into component within bounding box
 *          The weakness of (a) is that it is highly sensitive to holes
 *          within the c.c.  The weakness of (b) is that it can have
 *          arbitrarily large intrusions into the c.c.  Method (c) tests
 *          the integrity of the outer boundary of the c.c., with respect
 *          to the enclosing bounding box, so we use it.
 *      (2) This tests if the connected component within the box conforms
 *          to the box at all points on the periphery within @dist.
 *          Inside, at a distance from the box boundary that is greater
 *          than @dist, we don't care about the pixels in the c.c.
 *      (3) We can think of the conforming condition as follows:
 *          No pixel inside a distance @dist from the boundary
 *          can connect to the boundary through a path through the bg.
 *          To implement this, we need to do a flood fill.  We can go
 *          either from inside toward the boundary, or the other direction.
 *          It's easiest to fill from the boundary, and then verify that
 *          there are no filled pixels farther than @dist from the boundary.
 *}
function pixConformsToRectangle( pixs: PLPix; box: PLBox; dist: LongInt; pconforms: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                Extract rectangular region                   *
 *-------------------------------------------------------------*}

{*!
 *  pixClipRectangle()
 *
 *      Input:  pixs
 *              box  (requested clipping region; const)
 *              &boxc (<optional return> actual box of clipped region)
 *      Return: clipped pix, or null on error or if rectangle
 *              doesn't intersect pixs
 *
 *  Notes:
 *
 *  This should be simple, but there are choices to be made.
 *  The box is defined relative to the pix coordinates.  However,
 *  if the box is not contained within the pix, we have two choices:
 *
 *      (1) clip the box to the pix
 *      (2) make a new pix equal to the full box dimensions,
 *          but let rasterop do the clipping and positioning
 *          of the src with respect to the dest
 *
 *  Choice (2) immediately brings up the problem of what pixel values
 *  to use that were not taken from the src.  For example, on a grayscale
 *  image, do you want the pixels not taken from the src to be black
 *  or white or something else?  To implement choice 2, one needs to
 *  specify the color of these extra pixels.
 *
 *  So we adopt (1), and clip the box first, if necessary,
 *  before making the dest pix and doing the rasterop.  But there
 *  is another issue to consider.  If you want to paste the
 *  clipped pix back into pixs, it must be properly aligned, and
 *  it is necessary to use the clipped box for alignment.
 *  Accordingly, this function has a third (optional) argument, which is
 *  the input box clipped to the src pix.
 *}
function pixClipRectangle( pixs: PLPix; box: PLBox; pboxc: PPLBox ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixClipMasked()
 *
 *      Input:  pixs (1, 2, 4, 8, 16, 32 bpp; colormap ok)
 *              pixm  (clipping mask, 1 bpp)
 *              x, y (origin of clipping mask relative to pixs)
 *              outval (val to use for pixels that are outside the mask)
 *      Return: pixd, (clipped pix) or null on error or if pixm doesn't
 *              intersect pixs
 *
 *  Notes:
 *      (1) If pixs has a colormap, it is preserved in pixd.
 *      (2) The depth of pixd is the same as that of pixs.
 *      (3) If the depth of pixs is 1, use @outval = 0 for white background
 *          and 1 for black; otherwise, use the max value for white
 *          and 0 for black.  If pixs has a colormap, the max value for
 *          @outval is 0xffffffff; otherwise, it is 2^d - 1.
 *      (4) When using 1 bpp pixs, this is a simple clip and
 *          blend operation.  For example, if both pix1 and pix2 are
 *          black text on white background, and you want to OR the
 *          fg on the two images, let pixm be the inverse of pix2.
 *          Then the operation takes all of pix1 that's in the bg of
 *          pix2, and for the remainder (which are the pixels
 *          corresponding to the fg of the pix2), paint them black
 *          (1) in pix1.  The function call looks like
 *             pixClipMasked(pix2, pixInvert(pix1, pix1), x, y, 1);
 *}
function pixClipMasked( pixs, pixm: PLPix; x, y: LongInt; outval: LongWord ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixResizeToMatch()
 *
 *      Input:  pixs (1, 2, 4, 8, 16, 32 bpp; colormap ok)
 *              pixt  (can be null; we use only the size)
 *              w, h (ignored if pixt is defined)
 *      Return: pixd (resized to match) or null on error
 *
 *  Notes:
 *      (1) This resizes pixs to make pixd, without scaling, by either
 *          cropping or extending separately in both width and height.
 *          Extension is done by replicating the last row or column.
 *          This is useful in a situation where, due to scaling
 *          operations, two images that are expected to be the
 *          same size can differ slightly in each dimension.
 *      (2) You can use either an existing pixt or specify
 *          both @w and @h.  If pixt is defined, the values
 *          in @w and @h are ignored.
 *      (3) If pixt is larger than pixs (or if w and/or d is larger
 *          than the dimension of pixs, replicate the outer row and
 *          column of pixels in pixs into pixd.
 *}
function pixResizeToMatch( pixs, pixt: PLPix; w, h: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *                           Clip to Foreground                        *
 *---------------------------------------------------------------------*}

{*!
 *  pixClipToForeground()
 *
 *      Input:  pixs (1 bpp)
 *              &pixd  (<optional return> clipped pix returned)
 *              &box   (<optional return> bounding box)
 *      Return: 0 if OK; 1 on error or if there are no fg pixels
 *
 *  Notes:
 *      (1) At least one of {&pixd, &box} must be specified.
 *      (2) If there are no fg pixels, the returned ptrs are null.
 *}
function pixClipToForeground( pixs: PLPix; ppixd: PPLPix; pbox: PPLBox ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixClipBoxToForeground()
 *
 *      Input:  pixs (1 bpp)
 *              boxs  (<optional> ; use full image if null)
 *              &pixd  (<optional return> clipped pix returned)
 *              &boxd  (<optional return> bounding box)
 *      Return: 0 if OK; 1 on error or if there are no fg pixels
 *
 *  Notes:
 *      (1) At least one of {&pixd, &boxd} must be specified.
 *      (2) If there are no fg pixels, the returned ptrs are null.
 *      (3) Do not use &pixs for the 3rd arg or &boxs for the 4th arg;
 *          this will leak memory.
 *}
function pixClipBoxToForeground( pixs: PLPix; boxs: PLBox; ppixd: PPLPix; pboxd: PPLBox ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixScanForForeground()
 *
 *      Input:  pixs (1 bpp)
 *              box  (<optional> within which the search is conducted)
 *              scanflag (direction of scan; e.g., L_FROM_LEFT)
 *              &loc (location in scan direction of first black pixel)
 *      Return: 0 if OK; 1 on error or if no fg pixels are found
 *
 *  Notes:
 *      (1) If there are no fg pixels, the position is set to 0.
 *          Caller must check the return value!
 *      (2) Use @box == NULL to scan from edge of pixs
 *}
function pixScanForForeground( pixs: PLPix; box: PLBox; scanflag: LongInt; ploc: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixClipBoxToEdges()
 *
 *      Input:  pixs (1 bpp)
 *              boxs  (<optional> ; use full image if null)
 *              lowthresh (threshold to choose clipping location)
 *              highthresh (threshold required to find an edge)
 *              maxwidth (max allowed width between low and high thresh locs)
 *              factor (sampling factor along pixel counting direction)
 *              &pixd  (<optional return> clipped pix returned)
 *              &boxd  (<optional return> bounding box)
 *      Return: 0 if OK; 1 on error or if a fg edge is not found from
 *              all four sides.
 *
 *  Notes:
 *      (1) At least one of {&pixd, &boxd} must be specified.
 *      (2) If there are no fg pixels, the returned ptrs are null.
 *      (3) This function attempts to locate rectangular "image" regions
 *          of high-density fg pixels, that have well-defined edges
 *          on the four sides.
 *      (4) Edges are searched for on each side, iterating in order
 *          from left, right, top and bottom.  As each new edge is
 *          found, the search box is resized to use that location.
 *          Once an edge is found, it is held.  If no more edges
 *          are found in one iteration, the search fails.
 *      (5) See pixScanForEdge() for usage of the thresholds and @maxwidth.
 *      (6) The thresholds must be at least 1, and the low threshold
 *          cannot be larger than the high threshold.
 *      (7) If the low and high thresholds are both 1, this is equivalent
 *          to pixClipBoxToForeground().
 *}
function pixClipBoxToEdges( pixs: PLPix; boxs: PLBox; lowthresh, highthresh, maxwidth, factor: LongInt; ppixd: PPLPix; pboxd: PPLBox ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixScanForEdge()
 *
 *      Input:  pixs (1 bpp)
 *              box  (<optional> within which the search is conducted)
 *              lowthresh (threshold to choose clipping location)
 *              highthresh (threshold required to find an edge)
 *              maxwidth (max allowed width between low and high thresh locs)
 *              factor (sampling factor along pixel counting direction)
 *              scanflag (direction of scan; e.g., L_FROM_LEFT)
 *              &loc (location in scan direction of first black pixel)
 *      Return: 0 if OK; 1 on error or if the edge is not found
 *
 *  Notes:
 *      (1) If there are no fg pixels, the position is set to 0.
 *          Caller must check the return value!
 *      (2) Use @box == NULL to scan from edge of pixs
 *      (3) As the scan progresses, the location where the sum of
 *          pixels equals or excees @lowthresh is noted (loc).  The
 *          scan is stopped when the sum of pixels equals or exceeds
 *          @highthresh.  If the scan distance between loc and that
 *          point does not exceed @maxwidth, an edge is found and
 *          its position is taken to be loc.  @maxwidth implicitly
 *          sets a minimum on the required gradient of the edge.
 *      (4) The thresholds must be at least 1, and the low threshold
 *          cannot be larger than the high threshold.
 *}
function pixScanForEdge( pixs: PLPix; box: PLBox; lowthresh, highthresh, maxwidth, factor, scanflag: LongInt; ploc: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *           Extract pixel averages and reversals along lines          *
 *---------------------------------------------------------------------*}

{*!
 *  pixExtractOnLine()
 *
 *      Input:  pixs (1 bpp or 8 bpp; no colormap)
 *              x1, y1 (one end point for line)
 *              x2, y2 (another end pt for line)
 *              factor (sampling; >= 1)
 *      Return: na (of pixel values along line), or null on error.
 *
 *  Notes:
 *      (1) Input end points are clipped to the pix.
 *      (2) If the line is either horizontal, or closer to horizontal
 *          than to vertical, the points will be extracted from left
 *          to right in the pix.  Likewise, if the line is vertical,
 *          or closer to vertical than to horizontal, the points will
 *          be extracted from top to bottom.
 *      (3) Can be used with numaCountReverals(), for example, to
 *          characterize the intensity smoothness along a line.
 *}
function pixExtractOnLine( pixs: PLPix; x1, y1, x2, y2, factor: LongInt ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixAverageOnLine()
 *
 *      Input:  pixs (1 bpp or 8 bpp; no colormap)
 *              x1, y1 (starting pt for line)
 *              x2, y2 (end pt for line)
 *              factor (sampling; >= 1)
 *      Return: average of pixel values along line, or null on error.
 *
 *  Notes:
 *      (1) The line must be either horizontal or vertical, so either
 *          y1 == y2 (horizontal) or x1 == x2 (vertical).
 *      (2) If horizontal, x1 must be <= x2.
 *          If vertical, y1 must be <= y2.
 *          characterize the intensity smoothness along a line.
 *      (3) Input end points are clipped to the pix.
 *}
function pixAverageOnLine( pixs: PLPix; x1, y1, x2, y2, factor: LongInt ) : Single; cdecl; external LIBLEPT;


{*!
 *  pixAverageIntensityProfile()
 *
 *      Input:  pixs (any depth; colormap OK)
 *              fract (fraction of image width or height to be used)
 *              dir (averaging direction: L_HORIZONTAL_LINE or L_VERTICAL_LINE)
 *              first, last (span of rows or columns to measure)
 *              factor1 (sampling along fast scan direction; >= 1)
 *              factor2 (sampling along slow scan direction; >= 1)
 *      Return: na (of reversal profile), or null on error.
 *
 *  Notes:
 *      (1) If d != 1 bpp, colormaps are removed and the result
 *          is converted to 8 bpp.
 *      (2) If @dir == L_HORIZONTAL_LINE, the intensity is averaged
 *          along each horizontal raster line (sampled by @factor1),
 *          and the profile is the array of these averages in the
 *          vertical direction between @first and @last raster lines,
 *          and sampled by @factor2.
 *      (3) If @dir == L_VERTICAL_LINE, the intensity is averaged
 *          along each vertical line (sampled by @factor1),
 *          and the profile is the array of these averages in the
 *          horizontal direction between @first and @last columns,
 *          and sampled by @factor2.
 *      (4) The averages are measured over the central @fract of the image.
 *          Use @fract == 1.0 to average across the entire width or height.
 *}
function pixAverageIntensityProfile( pixs: PLPix; fract: Single; dir, first, last, factor1, factor2: LongInt ) : PNumArray; cdecl; external LIBLEPT;


{*!
 *  pixReversalProfile()
 *
 *      Input:  pixs (any depth; colormap OK)
 *              fract (fraction of image width or height to be used)
 *              dir (profile direction: L_HORIZONTAL_LINE or L_VERTICAL_LINE)
 *              first, last (span of rows or columns to measure)
 *              minreversal (minimum change in intensity to trigger a reversal)
 *              factor1 (sampling along raster line (fast scan); >= 1)
 *              factor2 (sampling of raster lines (slow scan); >= 1)
 *      Return: na (of reversal profile), or null on error.
 *
 *  Notes:
 *      (1) If d != 1 bpp, colormaps are removed and the result
 *          is converted to 8 bpp.
 *      (2) If @dir == L_HORIZONTAL_LINE, the the reversals are counted
 *          along each horizontal raster line (sampled by @factor1),
 *          and the profile is the array of these sums in the
 *          vertical direction between @first and @last raster lines,
 *          and sampled by @factor2.
 *      (3) If @dir == L_VERTICAL_LINE, the the reversals are counted
 *          along each vertical column (sampled by @factor1),
 *          and the profile is the array of these sums in the
 *          horizontal direction between @first and @last columns,
 *          and sampled by @factor2.
 *      (4) For each row or column, the reversals are summed over the
 *          central @fract of the image.  Use @fract == 1.0 to sum
 *          across the entire width (of row) or height (of column).
 *      (5) @minreversal is the relative change in intensity that is
 *          required to resolve peaks and valleys.  A typical number for
 *          locating text in 8 bpp might be 50.  For 1 bpp, minreversal
 *          must be 1.
 *      (6) The reversal profile is simply the number of reversals
 *          in a row or column, vs the row or column index.
 *}
function pixReversalProfile( pixs: PLPix; fract: Single; dir, first, last, minreversal, factor1, factor2: LongInt ) : PNumArray; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *                     Rank row and column transforms                  *
 *---------------------------------------------------------------------*}

{*!
 *  pixRankRowTransform()
 *
 *      Input:  pixs (8 bpp; no colormap)
 *      Return: pixd (with pixels sorted in each row, from
 *                    min to max value)
 *
 * Notes:
 *     (1) The time is O(n) in the number of pixels and runs about
 *         100 Mpixels/sec on a 3 GHz machine.
 *}
function pixRankRowTransform( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRankColumnTransform()
 *
 *      Input:  pixs (8 bpp; no colormap)
 *      Return: pixd (with pixels sorted in each column, from
 *                    min to max value)
 *
 * Notes:
 *     (1) The time is O(n) in the number of pixels and runs about
 *         50 Mpixels/sec on a 3 GHz machine.
 *}
function pixRankColumnTransform( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


