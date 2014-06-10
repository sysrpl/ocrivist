{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  skew.c                                                            PASCAL
 *
 *      Top-level deskew interfaces
 *          PIX       *pixDeskew()                                      X
 *          PIX       *pixFindSkewAndDeskew()
 *          PIX       *pixDeskewGeneral()                               X
 *
 *      Top-level angle-finding interface
 *          l_int32    pixFindSkew()                                    X
 *
 *      Basic angle-finding functions
 *          l_int32    pixFindSkewSweep()
 *          l_int32    pixFindSkewSweepAndSearch()
 *          l_int32    pixFindSkewSweepAndSearchScore()
 *          l_int32    pixFindSkewSweepAndSearchScorePivot()
 *
 *      Search over arbitrary range of angles in orthogonal directions
 *          l_int32    pixFindSkewOrthogonalRange()
 *
 *      Differential square sum function for scoring
 *          l_int32    pixFindDifferentialSquareSum()
 *
 *      Measures of variance of row sums
 *          l_int32    pixFindNormalizedSquareSum()
 *
 *
 *      ==============================================================
 *      Page skew detection
 *
 *      Skew is determined by pixel profiles, which are computed
 *      as pixel sums along the raster line for each line in the
 *      image.  By vertically shearing the image by a given angle,
 *      the sums can be computed quickly along the raster lines
 *      rather than along lines at that angle.  The score is
 *      computed from these line sums by taking the square of
 *      the DIFFERENCE between adjacent line sums, summed over
 *      all lines.  The skew angle is then found as the angle
 *      that maximizes the score.  The actual computation for
 *      any sheared image is done in the function
 *      pixFindDifferentialSquareSum().
 *
 *      The search for the angle that maximizes this score is
 *      most efficiently performed by first sweeping coarsely
 *      over angles, using a significantly reduced image (say, 4x
 *      reduction), to find the approximate maximum within a half
 *      degree or so, and then doing an interval-halving binary
 *      search at higher resolution to get the skew angle to
 *      within 1/20 degree or better.
 *
 *      The differential signal is used (rather than just using
 *      that variance of line sums) because it rejects the
 *      background noise due to total number of black pixels,
 *      and has maximum contributions from the baselines and
 *      x-height lines of text when the textlines are aligned
 *      with the raster lines.  It also works well in multicolumn
 *      pages where the textlines do not line up across columns.
 *
 *      The method is fast, accurate to within an angle (in radians)
 *      of approximately the inverse width in pixels of the image,
 *      and will work on a surprisingly small amount of text data
 *      (just a couple of text lines).  Consequently, it can
 *      also be used to find local skew if the skew were to vary
 *      significantly over the page.  Local skew determination
 *      is not very important except for locating lines of
 *      handwritten text that may be mixed with printed text.
 *}

{*-----------------------------------------------------------------------*
 *                       Top-level deskew interfaces                     *
 *-----------------------------------------------------------------------*/
/*!
 *  pixDeskew()
 *
 *      Input:  pixs (any depth)
 *              redsearch (for binary search: reduction factor = 1, 2 or 4;
 *                         use 0 for default)
 *      Return: pixd (deskewed pix), or null on error
 *
 *  Notes:
 *      (1) This binarizes if necessary and finds the skew angle.  If the
 *          angle is large enough and there is sufficient confidence,
 *          it returns a deskewed image; otherwise, it returns a clone.
 *}
function pixDeskew( pixs: PLPix; redsearch: Integer ): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixDeskewGeneral()
 *
 *      Input:  pixs  (any depth)
 *              redsweep  (for linear search: reduction factor = 1, 2 or 4;
 *                         use 0 for default)
 *              sweeprange (in degrees in each direction from 0;
 *                          use 0.0 for default)
 *              sweepdelta (in degrees; use 0.0 for default)
 *              redsearch  (for binary search: reduction factor = 1, 2 or 4;
 *                          use 0 for default;)
 *              thresh (for binarizing the image; use 0 for default)
 *              &angle   (<optional return> angle required to deskew,
 *                        in degrees; use NULL to skip)
 *              &conf    (<optional return> conf value is ratio
 *                        of max/min scores; use NULL to skip)
 *      Return: pixd (deskewed pix), or null on error
 *
 *  Notes:
 *      (1) This binarizes if necessary and finds the skew angle.  If the
 *          angle is large enough and there is sufficient confidence,
 *          it returns a deskewed image; otherwise, it returns a clone.
 *}
function pixDeskewGeneral( pixs: PLPix; redsweep: integer; sweeprange, sweepdelta: Single;
                           redsearch, thresh: Integer; pangle, pconf: PSingle ): PLPix; cdecl; external LIBLEPT;

{*-----------------------------------------------------------------------*
 *                  Simple top-level angle-finding interface             *
 *-----------------------------------------------------------------------*}
{*!
 *  pixFindSkew()
 *
 *      Input:  pixs  (1 bpp)
 *              @angle   (<return> angle required to deskew, in degrees)
 *              @conf    (<return> confidence value is ratio max/min scores)
 *      Return: 0 if OK, 1 on error or if angle measurment not valid
 *
 *  Notes:
 *      (1) This is a simple high-level interface, that uses default
 *          values of the parameters for reasonable speed and accuracy.
 *      (2) The angle returned is the negative of the skew angle of
 *          the image.  It is the angle required for deskew.
 *          Clockwise rotations are positive angles.
 *}
function pixFindSkew (pixs: PLPix; pangle, pconf: PLongint): Longint; cdecl; external LIBLEPT;

