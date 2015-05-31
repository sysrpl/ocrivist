{ This file is an automated translation to pascal of colorseg.c               }
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
 *  colorseg.c
 *
 *    Unsupervised color segmentation
 *
 *               PIX     *pixColorSegment()
 *               PIX     *pixColorSegmentCluster()
 *       static  l_int32  pixColorSegmentTryCluster()
 *               l_int32  pixAssignToNearestColor()
 *               l_int32  pixColorSegmentClean()
 *               l_int32  pixColorSegmentRemoveColors()
 *}

{* Maximum allowed iterations in Phase 1. *}
const MAX_ALLOWED_ITERATIONS : LongInt = 20;     

{* Factor by which max dist is increased on each iteration *}
const DIST_EXPAND_FACT : Single = 1.3;     

    {* Octcube division level for computing nearest colormap color using LUT.
     * Using 4 should suffice for up to 50 - 100 colors, and it is
     * very fast.  Using 5 takes 8 times as long to set up the LUT
     * for little perceptual gain, even with 100 colors. *}
const LEVEL_IN_OCTCUBE : LongInt = 4;     

{*------------------------------------------------------------------*
 *                 Unsupervised color segmentation                  *
 *------------------------------------------------------------------*}

{*!
 *  pixColorSegment()
 *
 *      Input:  pixs  (32 bpp; 24-bit color)
 *              maxdist (max euclidean dist to existing cluster)
 *              maxcolors (max number of colors allowed in first pass)
 *              selsize (linear size of sel for closing to remove noise)
 *              finalcolors (max number of final colors allowed after 4th pass)
 *      Return: pixd (8 bit with colormap), or null on error
 *
 *  Color segmentation proceeds in four phases:
 *
 *  Phase 1:  pixColorSegmentCluster()
 *  The image is traversed in raster order.  Each pixel either
 *  becomes the representative for a new cluster or is assigned to an
 *  existing cluster.  Assignment is greedy.  The data is stored in
 *  a colormapped image.  Three auxiliary arrays are used to hold
 *  the colors of the representative pixels, for fast lookup.
 *  The average color in each cluster is computed.
 *
 *  Phase 2.  pixAssignToNearestColor()
 *  A second (non-greedy) clustering pass is performed, where each pixel
 *  is assigned to the nearest cluster (average).  We also keep track
 *  of how many pixels are assigned to each cluster.
 *
 *  Phase 3.  pixColorSegmentClean()
 *  For each cluster, starting with the largest, do a morphological
 *  closing to eliminate small components within larger ones.
 *
 *  Phase 4.  pixColorSegmentRemoveColors()
 *  Eliminate all colors except the most populated 'finalcolors'.
 *  Then remove unused colors from the colormap, and reassign those
 *  pixels to the nearest remaining cluster, using the original pixel values.
 *
 *  Notes:
 *      (1) The goal is to generate a small number of colors.
 *          Typically this would be specified by 'finalcolors',
 *          a number that would be somewhere between 3 and 6.
 *          The parameter 'maxcolors' specifies the maximum number of
 *          colors generated in the first phase.  This should be
 *          larger than finalcolors, perhaps twice as large.
 *          If more than 'maxcolors' are generated in the first phase
 *          using the input 'maxdist', the distance is repeatedly
 *          increased by a multiplicative factor until the condition
 *          is satisfied.  The implicit relation between 'maxdist'
 *          and 'maxcolors' is thus adjusted programmatically.
 *      (2) As a very rough guideline, given a target value of 'finalcolors',
 *          here are approximate values of 'maxdist' and 'maxcolors'
 *          to start with:
 *
 *               finalcolors    maxcolors    maxdist
 *               -----------    ---------    -------
 *                   3             6          100
 *                   4             8           90
 *                   5            10           75
 *                   6            12           60
 *
 *          For a given number of finalcolors, if you use too many
 *          maxcolors, the result will be noisy.  If you use too few,
 *          the result will be a relatively poor assignment of colors.
 *}
function pixColorSegment( pixs: PLPix; maxdist, maxcolors, selsize, finalcolors: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixColorSegmentCluster()
 *
 *      Input:  pixs  (32 bpp; 24-bit color)
 *              maxdist (max euclidean dist to existing cluster)
 *              maxcolors (max number of colors allowed in first pass)
 *      Return: pixd (8 bit with colormap), or null on error
 *
 *  Notes:
 *      (1) This is phase 1.  See description in pixColorSegment().
 *      (2) Greedy unsupervised classification.  If the limit 'maxcolors'
 *          is exceeded, the computation is repeated with a larger
 *          allowed cluster size.
 *      (3) On each successive iteration, 'maxdist' is increased by a
 *          constant factor.  See comments in pixColorSegment() for
 *          a guideline on parameter selection.
 *          Note that the diagonal of the 8-bit rgb color cube is about
 *          440, so for 'maxdist' = 440, you are guaranteed to get 1 color!
 *}
function pixColorSegmentCluster( pixs: PLPix; maxdist, maxcolors: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixColorSegmentTryCluster()
 *
 *      Input:  pixd
 *              pixs
 *              maxdist
 *              maxcolors
 *      Return: 0 if OK, 1 on error
 *
 *  Note: This function should only be called from pixColorSegCluster()
 *}

{*!
 *  pixAssignToNearestColor()
 *
 *      Input:  pixd  (8 bpp, colormapped)
 *              pixs  (32 bpp; 24-bit color)
 *              pixm  (<optional> 1 bpp)
 *              level (of octcube used for finding nearest color in cmap)
 *              countarray (<optional> ptr to array, in which we can store
 *                          the number of pixels found in each color in
 *                          the colormap in pixd)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This is used in phase 2 of color segmentation, where pixs
 *          is the original input image to pixColorSegment(), and
 *          pixd is the colormapped image returned from
 *          pixColorSegmentCluster().  It is also used, with a mask,
 *          in phase 4.
 *      (2) This is an in-place operation.
 *      (3) The colormap in pixd is unchanged.
 *      (4) pixs and pixd must be the same size (w, h).
 *      (5) The selection mask pixm can be null.  If it exists, it must
 *          be the same size as pixs and pixd, and only pixels
 *          corresponding to fg in pixm are assigned.  Set to
 *          NULL if all pixels in pixd are to be assigned.
 *      (6) The countarray can be null.  If it exists, it is pre-allocated
 *          and of a size at least equal to the size of the colormap in pixd.
 *      (7) This does a best-fit (non-greedy) assignment of pixels to
 *          existing clusters.  Specifically, it assigns each pixel
 *          in pixd to the color index in the pixd colormap that has a
 *          color closest to the corresponding rgb pixel in pixs.
 *      (8) 'level' is the octcube level used to quickly find the nearest
 *          color in the colormap for each pixel.  For color segmentation,
 *          this parameter is set to LEVEL_IN_OCTCUBE.
 *      (9) We build a mapping table from octcube to colormap index so
 *          that this function can run in a time (otherwise) independent
 *          of the number of colors in the colormap.  This avoids a
 *          brute-force search for the closest colormap color to each
 *          pixel in the image.
 *}
function pixAssignToNearestColor( pixd, pixs, pixm: PLPix; level: LongInt; countarray: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixColorSegmentClean()
 *
 *      Input:  pixs  (8 bpp, colormapped)
 *              selsize (for closing)
 *              countarray (ptr to array containing the number of pixels
 *                          found in each color in the colormap)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This operation is in-place.
 *      (2) This is phase 3 of color segmentation.  It is the first
 *          part of a two-step noise removal process.  Colors with a
 *          large population are closed first; this operation absorbs
 *          small sets of intercolated pixels of a different color.
 *}
function pixColorSegmentClean( pixs: PLPix; selsize: LongInt; countarray: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixColorSegmentRemoveColors()
 *
 *      Input:  pixd  (8 bpp, colormapped)
 *              pixs  (32 bpp rgb, with initial pixel values)
 *              finalcolors (max number of colors to retain)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This operation is in-place.
 *      (2) This is phase 4 of color segmentation, and the second part
 *          of the 2-step noise removal.  Only 'finalcolors' different
 *          colors are retained, with colors with smaller populations
 *          being replaced by the nearest color of the remaining colors.
 *          For highest accuracy, for pixels that are being replaced,
 *          we find the nearest colormap color  to the original rgb color.
 *}
function pixColorSegmentRemoveColors( pixd, pixs: PLPix; finalcolors: LongInt ) : LongInt; cdecl; external LIBLEPT;


