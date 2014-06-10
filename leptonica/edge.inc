{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  edge.c                                                    PASCAL
 *
 *      Sobel edge detecting filter
 *          PIX      *pixSobelEdgeFilter()                      X
 *
 *      Two-sided edge gradient filter
 *          PIX      *pixTwoSidedEdgeFilter()
 *
 *      Measurement of edge smoothness
 *          l_int32   pixMeasureEdgeSmoothness()
 *          NUMA     *pixGetEdgeProfile()
 *          l_int32   pixGetLastOffPixelInRun()
 *          l_int32   pixGetLastOnPixelInRun()
 *
 *
 *  The Sobel edge detector uses these two simple gradient filters.
 *
 *       1    2    1             1    0   -1
 *       0    0    0             2    0   -2
 *      -1   -2   -1             1    0   -1
 *
 *      (horizontal)             (vertical)
 *
 *  To use both the vertical and horizontal filters, set the orientation
 *  flag to L_ALL_EDGES; this sums the abs. value of their outputs,
 *  clipped to 255.
 *
 *  See comments below for displaying the resulting image with
 *  the edges dark, both for 8 bpp and 1 bpp.
 *}

{*----------------------------------------------------------------------*
 *                    Sobel edge detecting filter                       *
 *----------------------------------------------------------------------*}
{*!
 *  pixSobelEdgeFilter()
 *
 *      Input:  pixs (8 bpp; no colormap)
 *              orientflag (L_HORIZONTAL_EDGES, L_VERTICAL_EDGES, L_ALL_EDGES)
 *      Return: pixd (8 bpp, edges are brighter), or null on error
 *
 *  Notes:
 *      (1) Invert pixd to see larger gradients as darker (grayscale).
 *      (2) To generate a binary image of the edges, threshold
 *          the result using pixThresholdToBinary().  If the high
 *          edge values are to be fg (1), invert after running
 *          pixThresholdToBinary().
 *      (3) Label the pixels as follows:
 *              1    4    7
 *              2    5    8
 *              3    6    9
 *          Read the data incrementally across the image and unroll
 *          the loop.
 *      (4) This runs at about 45 Mpix/sec on a 3 GHz processor.
 *}
function pixSobelEdgeFilter ( pixs: PLPix; orientflag: Integer ): PLPix;  cdecl; external LIBLEPT;
