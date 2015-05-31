{ This file is an automated translation to pascal of bilinear.c               }
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
 *  bilinear.c
 *
 *      Bilinear (4 pt) image transformation using a sampled
 *      (to nearest integer) transform on each dest point
 *           PIX      *pixBilinearSampledPta()
 *           PIX      *pixBilinearSampled()
 *
 *      Bilinear (4 pt) image transformation using interpolation
 *      (or area mapping) for anti-aliasing images that are
 *      2, 4, or 8 bpp gray, or colormapped, or 32 bpp RGB
 *           PIX      *pixBilinearPta()
 *           PIX      *pixBilinear()
 *           PIX      *pixBilinearPtaColor()
 *           PIX      *pixBilinearColor()
 *           PIX      *pixBilinearPtaGray()
 *           PIX      *pixBilinearGray()
 *
 *      Bilinear transform including alpha (blend) component and gamma xform
 *           PIX      *pixBilinearPtaWithAlpha()
 *           PIX      *pixBilinearPtaGammaXform()
 *
 *      Bilinear coordinate transformation
 *           l_int32   getBilinearXformCoeffs()
 *           l_int32   bilinearXformSampledPt()
 *           l_int32   bilinearXformPt()
 *
 *      A bilinear transform can be specified as a specific functional
 *      mapping between 4 points in the source and 4 points in the dest.
 *      It can be used as an approximation to a (nonlinear) projective
 *      transform, because for small warps it is very similar and
 *      it is more stable.  (Projective transforms have a division
 *      by a quantity that can get arbitrarily small.)
 *
 *      We give both a bilinear coordinate transformation and
 *      a bilinear image transformation.
 *
 *      For the former, we ask for the coordinate value (x',y')
 *      in the transformed space for any point (x,y) in the original
 *      space.  The coefficients of the transformation are found by
 *      solving 8 simultaneous equations for the 8 coordinates of
 *      the 4 points in src and dest.  The transformation can then
 *      be used to compute the associated image transform, by
 *      computing, for each dest pixel, the relevant pixel(s) in
 *      the source.  This can be done either by taking the closest
 *      src pixel to each transformed dest pixel ("sampling") or
 *      by doing an interpolation and averaging over 4 source
 *      pixels with appropriate weightings ("interpolated").
 *
 *      A typical application would be to remove some of the
 *      keystoning due to a projective transform in the imaging system.
 *
 *      The bilinear transform is given by specifying two equations:
 *
 *          x' = ax + by + cxy + d
 *          y' = ex + fy + gxy + h
 *
 *      where the eight coefficients have been computed from four
 *      sets of these equations, each for two corresponding data pts.
 *      In practice, for each point (x,y) in the dest image, this
 *      equation is used to compute the corresponding point (x',y')
 *      in the src.  That computed point in the src is then used
 *      to determine the dest value in one of two ways:
 *
 *       - sampling: take the value of the src pixel in which this
 *                   point falls
 *       - interpolation: take appropriate linear combinations of the
 *                        four src pixels that this dest pixel would
 *                        overlap, with the coefficients proportional
 *                        to the amount of overlap
 *
 *      For small warp, like rotation, area mapping in the
 *      interpolation is equivalent to linear interpolation.
 *
 *      Typical relative timing of transforms (sampled = 1.0):
 *      8 bpp:   sampled        1.0
 *               interpolated   1.6
 *      32 bpp:  sampled        1.0
 *               interpolated   1.8
 *      Additionally, the computation time/pixel is nearly the same
 *      for 8 bpp and 32 bpp, for both sampled and interpolated.
 *}

{*-------------------------------------------------------------*
 *             Sampled bilinear image transformation           *
 *-------------------------------------------------------------*}

{*!
 *  pixBilinearSampledPta()
 *
 *      Input:  pixs (all depths)
 *              ptad  (4 pts of final coordinate space)
 *              ptas  (4 pts of initial coordinate space)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Brings in either black or white pixels from the boundary.
 *      (2) Retains colormap, which you can do for a sampled transform..
 *      (3) No 3 of the 4 points may be collinear.
 *      (4) For 8 and 32 bpp pix, better quality is obtained by the
 *          somewhat slower pixBilinearPta().  See that
 *          function for relative timings between sampled and interpolated.
 *}
function pixBilinearSampledPta( pixs: PLPix; ptad, ptas: PPointArray; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearSampled()
 *
 *      Input:  pixs (all depths)
 *              vc  (vector of 8 coefficients for bilinear transformation)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Brings in either black or white pixels from the boundary.
 *      (2) Retains colormap, which you can do for a sampled transform..
 *      (3) For 8 or 32 bpp, much better quality is obtained by the
 *          somewhat slower pixBilinear().  See that function
 *          for relative timings between sampled and interpolated.
 *}
function pixBilinearSampled( pixs: PLPix; vc: PSingle; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *            Interpolated bilinear image transformation             *
 *---------------------------------------------------------------------*}

{*!
 *  pixBilinearPta()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              ptad  (4 pts of final coordinate space)
 *              ptas  (4 pts of initial coordinate space)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Brings in either black or white pixels from the boundary
 *      (2) Removes any existing colormap, if necessary, before transforming
 *}
function pixBilinearPta( pixs: PLPix; ptad, ptas: PPointArray; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinear()
 *
 *      Input:  pixs (all depths; colormap ok)
 *              vc  (vector of 8 coefficients for bilinear transformation)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Brings in either black or white pixels from the boundary
 *      (2) Removes any existing colormap, if necessary, before transforming
 *}
function pixBilinear( pixs: PLPix; vc: PSingle; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearPtaColor()
 *
 *      Input:  pixs (32 bpp)
 *              ptad  (4 pts of final coordinate space)
 *              ptas  (4 pts of initial coordinate space)
 *              colorval (e.g., 0 to bring in BLACK, 0xffffff00 for WHITE)
 *      Return: pixd, or null on error
 *}
function pixBilinearPtaColor( pixs: PLPix; ptad, ptas: PPointArray; colorval: LongWord ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearColor()
 *
 *      Input:  pixs (32 bpp)
 *              vc  (vector of 8 coefficients for bilinear transformation)
 *              colorval (e.g., 0 to bring in BLACK, 0xffffff00 for WHITE)
 *      Return: pixd, or null on error
 *}
function pixBilinearColor( pixs: PLPix; vc: PSingle; colorval: LongWord ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearPtaGray()
 *
 *      Input:  pixs (8 bpp)
 *              ptad  (4 pts of final coordinate space)
 *              ptas  (4 pts of initial coordinate space)
 *              grayval (0 to bring in BLACK, 255 for WHITE)
 *      Return: pixd, or null on error
 *}
function pixBilinearPtaGray( pixs: PLPix; ptad, ptas: PPointArray; grayval: Byte ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearGray()
 *
 *      Input:  pixs (8 bpp)
 *              vc  (vector of 8 coefficients for bilinear transformation)
 *              grayval (0 to bring in BLACK, 255 for WHITE)
 *      Return: pixd, or null on error
 *}
function pixBilinearGray( pixs: PLPix; vc: PSingle; grayval: Byte ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------------------*
 *  Bilinear transform including alpha (blend) component and gamma xform   *
 *-------------------------------------------------------------------------*}

{*!
 *  pixBilinearPtaWithAlpha()
 *
 *      Input:  pixs (32 bpp rgb)
 *              ptad  (4 pts of final coordinate space)
 *              ptas  (4 pts of initial coordinate space)
 *              pixg (<optional> 8 bpp, can be null)
 *              fract (between 0.0 and 1.0, with 0.0 fully transparent
 *                     and 1.0 fully opaque)
 *              border (of pixels added to capture transformed source pixels)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) The alpha channel is transformed separately from pixs,
 *          and aligns with it, being fully transparent outside the
 *          boundary of the transformed pixs.  For pixels that are fully
 *          transparent, a blending function like pixBlendWithGrayMask()
 *          will give zero weight to corresponding pixels in pixs.
 *      (2) If pixg is NULL, it is generated as an alpha layer that is
 *          partially opaque, using @fract.  Otherwise, it is cropped
 *          to pixs if required and @fract is ignored.  The alpha channel
 *          in pixs is never used.
 *      (3) Colormaps are removed.
 *      (4) When pixs is transformed, it doesn't matter what color is brought
 *          in because the alpha channel will be transparent (0) there.
 *      (5) To avoid losing source pixels in the destination, it may be
 *          necessary to add a border to the source pix before doing
 *          the bilinear transformation.  This can be any non-negative number.
 *      (6) The input @ptad and @ptas are in a coordinate space before
 *          the border is added.  Internally, we compensate for this
 *          before doing the bilinear transform on the image after
 *          the border is added.
 *      (7) The default setting for the border values in the alpha channel
 *          is 0 (transparent) for the outermost ring of pixels and
 *          (0.5 * fract * 255) for the second ring.  When blended over
 *          a second image, this
 *          (a) shrinks the visible image to make a clean overlap edge
 *              with an image below, and
 *          (b) softens the edges by weakening the aliasing there.
 *          Use l_setAlphaMaskBorder() to change these values.
 *}
function pixBilinearPtaWithAlpha( pixs: PLPix; ptad, ptas: PPointArray; pixg: PLPix; fract: Single; border: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixBilinearPtaGammaXform()
 *
 *      Input:  pixs (32 bpp rgb)
 *              gamma (gamma correction; must be > 0.0)
 *              ptad  (3 pts of final coordinate space)
 *              ptas  (3 pts of initial coordinate space)
 *              fract (between 0.0 and 1.0, with 1.0 fully transparent)
 *              border (of pixels to capture transformed source pixels)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This wraps a gamma/inverse-gamma photometric transform around
 *          pixBilinearPtaWithAlpha().
 *      (2) For usage, see notes in pixBilinearPtaWithAlpha() and
 *          pixGammaTRCWithAlpha().
 *      (3) The basic idea of a gamma/inverse-gamma transform is to remove
 *          any gamma correction before the bilinear transform, and restore
 *          it afterward.  The effects can be subtle, but important for
 *          some applications.  For example, using gamma > 1.0 will
 *          cause the dark areas to become somewhat lighter and slightly
 *          reduce aliasing effects when blending using the alpha channel.
 *}
function pixBilinearPtaGammaXform( pixs: PLPix; gamma: Single; ptad, ptas: PPointArray; fract: Single; border: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                Bilinear coordinate transformation           *
 *-------------------------------------------------------------*}

{*!
 *  getBilinearXformCoeffs()
 *
 *      Input:  ptas  (source 4 points; unprimed)
 *              ptad  (transformed 4 points; primed)
 *              &vc   (<return> vector of coefficients of transform)
 *      Return: 0 if OK; 1 on error
 *
 *  We have a set of 8 equations, describing the bilinear
 *  transformation that takes 4 points (ptas) into 4 other
 *  points (ptad).  These equations are:
 *
 *          x1' = c[0]*x1 + c[1]*y1 + c[2]*x1*y1 + c[3]
 *          y1' = c[4]*x1 + c[5]*y1 + c[6]*x1*y1 + c[7]
 *          x2' = c[0]*x2 + c[1]*y2 + c[2]*x2*y2 + c[3]
 *          y2' = c[4]*x2 + c[5]*y2 + c[6]*x2*y2 + c[7]
 *          x3' = c[0]*x3 + c[1]*y3 + c[2]*x3*y3 + c[3]
 *          y3' = c[4]*x3 + c[5]*y3 + c[6]*x3*y3 + c[7]
 *          x4' = c[0]*x4 + c[1]*y4 + c[2]*x4*y4 + c[3]
 *          y4' = c[4]*x4 + c[5]*y4 + c[6]*x4*y4 + c[7]
 *
 *  This can be represented as
 *
 *           AC = B
 *
 *  where B and C are column vectors
 *
 *         B = [ x1' y1' x2' y2' x3' y3' x4' y4' ]
 *         C = [ c[0] c[1] c[2] c[3] c[4] c[5] c[6] c[7] ]
 *
 *  and A is the 8x8 matrix
 *
 *             x1   y1   x1*y1   1   0    0      0     0
 *              0    0     0     0   x1   y1   x1*y1   1
 *             x2   y2   x2*y2   1   0    0      0     0
 *              0    0     0     0   x2   y2   x2*y2   1
 *             x3   y3   x3*y3   1   0    0      0     0
 *              0    0     0     0   x3   y3   x3*y3   1
 *             x4   y4   x4*y4   1   0    0      0     0
 *              0    0     0     0   x4   y4   x4*y4   1
 *
 *  These eight equations are solved here for the coefficients C.
 *
 *  These eight coefficients can then be used to find the mapping
 *  (x,y) --> (x',y'):
 *
 *           x' = c[0]x + c[1]y + c[2]xy + c[3]
 *           y' = c[4]x + c[5]y + c[6]xy + c[7]
 *
 *  that are implemented in bilinearXformSampledPt() and
 *  bilinearXFormPt().
 *}
function getBilinearXformCoeffs( ptas, ptad: PPointArray; pvc: PPSingle ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  bilinearXformSampledPt()
 *
 *      Input:  vc (vector of 8 coefficients)
 *              (x, y)  (initial point)
 *              (&xp, &yp)   (<return> transformed point)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This finds the nearest pixel coordinates of the transformed point.
 *      (2) It does not check ptrs for returned data!
 *}
function bilinearXformSampledPt( vc: PSingle; x, y: LongInt; pxp, pyp: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  bilinearXformPt()
 *
 *      Input:  vc (vector of 8 coefficients)
 *              (x, y)  (initial point)
 *              (&xp, &yp)   (<return> transformed point)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) This computes the floating point location of the transformed point.
 *      (2) It does not check ptrs for returned data!
 *}
function bilinearXformPt( vc: PSingle; x, y: LongInt; pxp, pyp: PSingle ) : LongInt; cdecl; external LIBLEPT;


