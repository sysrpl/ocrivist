{ This file is an automated translation to pascal of edge.c                   }
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
 *  edge.c
 *
 *      Sobel edge detecting filter
 *          PIX      *pixSobelEdgeFilter()
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
function pixSobelEdgeFilter( pixs: PLPix; orientflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *                   Two-sided edge gradient filter                     *
 *----------------------------------------------------------------------*}

{*!
 *  pixTwoSidedEdgeFilter()
 *
 *      Input:  pixs (8 bpp; no colormap)
 *              orientflag (L_HORIZONTAL_EDGES, L_VERTICAL_EDGES)
 *      Return: pixd (8 bpp, edges are brighter), or null on error
 *
 *  Notes:
 *      (1) For detecting vertical edges, this considers the
 *          difference of the central pixel from those on the left
 *          and right.  For situations where the gradient is the same
 *          sign on both sides, this computes and stores the minimum
 *          (absolute value of the) difference.  The reason for
 *          checking the sign is that we are looking for pixels within
 *          a transition.  By contrast, for single pixel noise, the pixel
 *          value is either larger than or smaller than its neighbors,
 *          so the gradient would change direction on each side.  Horizontal
 *          edges are handled similarly, looking for vertical gradients.
 *      (2) To generate a binary image of the edges, threshold
 *          the result using pixThresholdToBinary().  If the high
 *          edge values are to be fg (1), invert after running
 *          pixThresholdToBinary().
 *      (3) This runs at about 60 Mpix/sec on a 3 GHz processor.
 *          It is about 30% faster than Sobel, and the results are
 *          similar.
 *}
function pixTwoSidedEdgeFilter( pixs: PLPix; orientflag: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*----------------------------------------------------------------------*
 *                   Measurement of edge smoothness                     *
 *----------------------------------------------------------------------*}

{*!
 *  pixMeasureEdgeSmoothness()
 *
 *      Input:  pixs (1 bpp)
 *              side (L_FROM_LEFT, L_FROM_RIGHT, L_FROM_TOP, L_FROM_BOTTOM)
 *              minjump (minimum jump to be counted; >= 1)
 *              minreversal (minimum reversal size for new peak or valley)
 *              &jpl (<optional return> jumps/length: number of jumps,
 *                    normalized to length of component side)
 *              &jspl (<optional return> jumpsum/length: sum of all
 *                     sufficiently large jumps, normalized to length
 *                     of component side)
 *              &rpl (<optional return> reversals/length: number of
 *                    peak-to-valley or valley-to-peak reversals,
 *                    normalized to length of component side)
 *              debugfile (<optional> displays constructed edge; use NULL
 *                         for no output)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This computes three measures of smoothness of the edge of a
 *          connected component:
 *            * jumps/length: (jpl) the number of jumps of size >= @minjump,
 *              normalized to the length of the side
 *            * jump sum/length: (jspl) the sum of all jump lengths of
 *              size >= @minjump, normalized to the length of the side
 *            * reversals/length: (rpl) the number of peak <--> valley
 *              reversals, using @minreverse as a minimum deviation of
 *              the peak or valley from its preceeding extremum,
 *              normalized to the length of the side
 *      (2) The input pix should be a single connected component, but
 *          this is not required.
 *}
function pixMeasureEdgeSmoothness( pixs: PLPix; side, minjump, minreversal: LongInt; pjpl, pjspl, prpl: PSingle; debugfile: PChar ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetEdgeProfile()
 *
 *      Input:  pixs (1 bpp)
 *              side (L_FROM_LEFT, L_FROM_RIGHT, L_FROM_TOP, L_FROM_BOTTOM)
 *              debugfile (<optional> displays constructed edge; use NULL
 *                         for no output)
 *      Return: na (of fg edge pixel locations), or null on error
 *}
function pixGetEdgeProfile( pixs: PLPix; side: LongInt; debugfile: PChar ) : PNumArray; cdecl; external LIBLEPT;


{*
 *  pixGetLastOffPixelInRun()
 *
 *      Input:  pixs (1 bpp)
 *              x, y (starting location)
 *              direction (L_FROM_LEFT, L_FROM_RIGHT, L_FROM_TOP, L_FROM_BOTTOM)
 *              &loc (<return> location in scan direction coordinate
 *                    of last OFF pixel found)
 *      Return: na (of fg edge pixel locations), or null on error
 *
 *  Notes:
 *      (1) Search starts from the pixel at (x, y), which is OFF.
 *      (2) It returns the location in the scan direction of the last
 *          pixel in the current run that is OFF.
 *      (3) The interface for these pixel run functions is cleaner when
 *          you ask for the last pixel in the current run, rather than the
 *          first pixel of opposite polarity that is found, because the
 *          current run may go to the edge of the image, in which case
 *          no pixel of opposite polarity is found.
 *}
function pixGetLastOffPixelInRun( pixs: PLPix; x, y, direction: LongInt; ploc: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*
 *  pixGetLastOnPixelInRun()
 *
 *      Input:  pixs (1 bpp)
 *              x, y (starting location)
 *              direction (L_FROM_LEFT, L_FROM_RIGHT, L_FROM_TOP, L_FROM_BOTTOM)
 *              &loc (<return> location in scan direction coordinate
 *                    of first ON pixel found)
 *      Return: na (of fg edge pixel locations), or null on error
 *
 *  Notes:
 *      (1) Search starts from the pixel at (x, y), which is ON.
 *      (2) It returns the location in the scan direction of the last
 *          pixel in the current run that is ON.
 *}
function pixGetLastOnPixelInRun( pixs: PLPix; x, y, direction: LongInt; ploc: PLongint ) : LongInt; cdecl; external LIBLEPT;


