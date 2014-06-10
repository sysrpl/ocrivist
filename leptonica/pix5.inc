{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  pix5.c                                                        PASCAL
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
 *           PIX        *pixClipRectangle()                          X
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

{*-------------------------------------------------------------*
 *                Extract rectangular region                   *
 *-------------------------------------------------------------*/
/*!
 *  pixClipRectangle()
 *
 *      Input:  pixs
 *              box  (requested clipping region; const)
 *              @boxc (<optional return> actual box of clipped region)
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
function pixClipRectangle( pixs: PLPix; box: PLBox; pboxc: PPLBox): PLPix; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *                           Clip to Foreground                        *
 *---------------------------------------------------------------------*}
{*!
 *  pixClipToForeground()
 *
 *      Input:  pixs (1 bpp)
 *              @pixd  (<optional return> clipped pix returned)
 *              @box   (<optional return> bounding box)
 *      Return: 0 if OK; 1 on error or if there are no fg pixels
 *
 *  Notes:
 *      (1) At least one of @pixd, @box must be specified.
 *      (2) If there are no fg pixels, the returned ptrs are null.
 *}
function pixClipToForeground( pixs: PLPix; ppixd: PPLPix; pbox: PPLBox): Longint; cdecl; external LIBLEPT;

{*!
 *  pixClipBoxToForeground()
 *
 *      Input:  pixs (1 bpp)
 *              boxs  (<optional> ; use full image if null)
 *              @pixd  (<optional return> clipped pix returned)
 *              @boxd  (<optional return> bounding box)
 *      Return: 0 if OK; 1 on error or if there are no fg pixels
 *
 *  Notes:
 *      (1) At least one of {@pixd, @boxd} must be specified.
 *      (2) If there are no fg pixels, the returned ptrs are null.
 *      (3) Do not use &pixs for the 3rd arg or &boxs for the 4th arg;
 *          this will leak memory.
 *}
function pixClipBoxToForeground( pixs: PLPix; boxs: PLBox; ppixd: PPLPix; pboxd: PPLBox): Longint; cdecl; external LIBLEPT;

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
 function pixClipBoxToEdges( pixs: PLPix; boxs: PLBox; lowthresh, highthresh, maxwidth, factor: Longint; ppixd: PPLPix; pboxd: PPLBox ): Longint; cdecl; external LIBLEPT;

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
function pixScanForEdge( pixs: PLPix; boxs: PLBox; lowthresh, highthresh, maxwidth, factor, scanflag: Longint; ploc: PLongint ): Longint; cdecl; external LIBLEPT;
