{/*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  scale.c                                                     (TRANSLATED)
 *
 *         Top-level scaling
 *               PIX    *pixScale()     ***                          X
 *               PIX    *pixScaleToSize()     ***                    X
 *               PIX    *pixScaleGeneral()     ***
 *
 *         Linearly interpreted (usually up-) scaling
 *               PIX    *pixScaleLI()     ***
 *               PIX    *pixScaleColorLI()
 *               PIX    *pixScaleColor2xLI()   ***
 *               PIX    *pixScaleColor4xLI()   ***
 *               PIX    *pixScaleGrayLI()
 *               PIX    *pixScaleGray2xLI()
 *               PIX    *pixScaleGray4xLI()
 *
 *         Scaling by closest pixel sampling
 *               PIX    *pixScaleBySampling()                        X
 *               PIX    *pixScaleByIntSubsampling()
 *
 *         Fast integer factor subsampling RGB to gray and to binary
 *               PIX    *pixScaleRGBToGrayFast()                     X
 *               PIX    *pixScaleRGBToBinaryFast()
 *               PIX    *pixScaleGrayToBinaryFast()
 *
 *         Downscaling with (antialias) smoothing
 *               PIX    *pixScaleSmooth() ***                        X
 *               PIX    *pixScaleRGBToGray2()   [special 2x reduction to gray]
 *
 *         Downscaling with (antialias) area mapping
 *               PIX    *pixScaleAreaMap()     ***
 *               PIX    *pixScaleAreaMap2()
 *
 *         Binary scaling by closest pixel sampling
 *               PIX    *pixScaleBinary()
 *
 *         Scale-to-gray (1 bpp --> 8 bpp; arbitrary downscaling)
 *               PIX    *pixScaleToGray()
 *               PIX    *pixScaleToGrayFast()
 *
 *         Scale-to-gray (1 bpp --> 8 bpp; integer downscaling)
 *               PIX    *pixScaleToGray2()
 *               PIX    *pixScaleToGray3()
 *               PIX    *pixScaleToGray4()
 *               PIX    *pixScaleToGray6()
 *               PIX    *pixScaleToGray8()
 *               PIX    *pixScaleToGray16()
 *
 *         Scale-to-gray by mipmap(1 bpp --> 8 bpp, arbitrary reduction)
 *               PIX    *pixScaleToGrayMipmap()
 *
 *         Grayscale scaling using mipmap
 *               PIX    *pixScaleMipmap()
 *
 *         Replicated (integer) expansion (all depths)
 *               PIX    *pixExpandReplicate()
 *
 *         Upscale 2x followed by binarization
 *               PIX    *pixScaleGray2xLIThresh()
 *               PIX    *pixScaleGray2xLIDither()
 *
 *         Upscale 4x followed by binarization
 *               PIX    *pixScaleGray4xLIThresh()
 *               PIX    *pixScaleGray4xLIDither()
 *
 *         Grayscale downscaling using min and max
 *               PIX    *pixScaleGrayMinMax()
 *               PIX    *pixScaleGrayMinMax2()
 *
 *         Grayscale downscaling using rank value
 *               PIX    *pixScaleGrayRankCascade()
 *               PIX    *pixScaleGrayRank2()
 *
 *         RGB scaling including alpha (blend) component and gamma transform
 *               PIX    *pixScaleWithAlpha()   ***
 *               PIX    *pixScaleGammaXform()  ***
 *
 *  *** Note: these functions make an implicit assumption about RGB
 *            component ordering.
 *}

{*------------------------------------------------------------------*
 *                    Top level scaling dispatcher                  *
 *------------------------------------------------------------------*}
{*!
 *  pixScale()
 *
 *      Input:  pixs (1, 2, 4, 8, 16 and 32 bpp)
 *              scalex, scaley
 *      Return: pixd, or null on error
 *
 *  This function scales 32 bpp RGB; 2, 4 or 8 bpp palette color;
 *  2, 4, 8 or 16 bpp gray; and binary images.
 *
 *  When the input has palette color, the colormap is removed and
 *  the result is either 8 bpp gray or 32 bpp RGB, depending on whether
 *  the colormap has color entries.  Images with 2, 4 or 16 bpp are
 *  converted to 8 bpp.
 *
 *  Because pixScale() is meant to be a very simple interface to a
 *  number of scaling functions, including the use of unsharp masking,
 *  the type of scaling and the sharpening parameters are chosen
 *  by default.  Grayscale and color images are scaled using one
 *  of four methods, depending on the scale factors:
 *   (1) antialiased subsampling (lowpass filtering followed by
 *       subsampling, implemented here by area mapping), for scale factors
 *       less than 0.2
 *   (2) antialiased subsampling with sharpening, for scale factors
 *       between 0.2 and 0.7
 *   (3) linear interpolation with sharpening, for scale factors between
 *       0.7 and 1.4
 *   (4) linear interpolation without sharpening, for scale factors >= 1.4.
 *
 *  One could use subsampling for scale factors very close to 1.0,
 *  because it preserves sharp edges.  Linear interpolation blurs
 *  edges because the dest pixels will typically straddle two src edge
 *  pixels.  Subsmpling removes entire columns and rows, so the edge is
 *  not blurred.  However, there are two reasons for not doing this.
 *  First, it moves edges, so that a straight line at a large angle to
 *  both horizontal and vertical will have noticable kinks where
 *  horizontal and vertical rasters are removed.  Second, although it
 *  is very fast, you get good results on sharp edges by applying
 *  a sharpening filter.
 *
 *  For images with sharp edges, sharpening substantially improves the
 *  image quality for scale factors between about 0.2 and about 2.0.  However,
 *  the generic sharpening operation is about 3 times slower than linear
 *  interpolation, so there is a speed-vs-quality tradeoff.  (Note: the
 *  cases where the sharpening halfwidth is 1 or 2 have special
 *  implementations and are about twice as fast as the general case).
 *  When the scale factor is larger than 1.4, the cost, which is
 *  proportional to image area, is very large for the incremental
 *  quality improvement, so we cut off the use of sharpening at 1.4.
 *  For scale factors greater than 1.4, these high-level scaling
 *  functions only do linear interpolation.
 *
 *  Because sharpening is computationally expensive, we provide the
 *  option of not doing it.  To avoid sharpening, call pixScaleGeneral()
 *  with @sharpfract = 0.0.  pixScale() uses a small amount of
 *  of sharpening because it strengthens edge pixels that are weak
 *  due to anti-aliasing.  The sharpening factors are:
 *      * for scaling factors < 0.7:   sharpfract = 0.2    sharpwidth = 1
 *      * for scaling factors >= 0.7:  sharpfract = 0.4    sharpwidth = 2
 *
 *  The constraints that tie sharpening to the scale factor
 *  in pixScaleGeneral() can be circumvented by calling with
 *  @sharpfract = 0.0.  This can be followed by the sharpening of
 *  choice; e.g., pixUnsharpMasking().
 *
 *  Binary images are scaled by sampling the closest pixel, without
 *  any low-pass filtering (averaging of neighboring pixels).
 *  This will introduce aliasing for reductions, which can be
 *  prevented by using pixScaleToGray() instead.
 *
 *  *** Warning: implicit assumption about RGB component order
 *               for LI color scaling
 *}
function pixScale( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *               Downscaling with (antialias) smoothing             *
 *------------------------------------------------------------------*}
{*!
 *  pixScaleSmooth()
 *
 *      Input:  pixs (2, 4, 8 or 32 bpp; and 2, 4, 8 bpp with colormap)
 *              scalex, scaley (must both be < 0.7)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This function should only be used when the scale factors are less
 *          than or equal to 0.7 (i.e., more than about 1.42x reduction).
 *          If either scale factor is larger than 0.7, we issue a warning
 *          and invoke pixScale().
 *      (2) This works only on 2, 4, 8 and 32 bpp images, and if there is
 *          a colormap, it is removed by converting to RGB.  In other
 *          cases, we issue a warning and invoke pixScale().
 *      (3) It does simple (flat filter) convolution, with a filter size
 *          commensurate with the amount of reduction, to avoid antialiasing.
 *      (4) It does simple subsampling after smoothing, which is appropriate
 *          for this range of scaling.  Linear interpolation gives essentially
 *          the same result with more computation for these scale factors,
 *          so we don't use it.
 *      (5) The result is the same as doing a full block convolution followed by
 *          subsampling, but this is faster because the results of the block
 *          convolution are only computed at the subsampling locations.
 *          In fact, the computation time is approximately independent of
 *          the scale factor, because the convolution kernel is adjusted
 *          so that each source pixel is summed approximately once.
 *
 *  *** Warning: implicit assumption about RGB component ordering ***
 *}
function pixScaleSmooth( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixScaleToSize()
 *
 *      Input:  pixs (1, 2, 4, 8, 16 and 32 bpp)
 *              wd  (target width; use 0 if using height as target)
 *              hd  (target height; use 0 if using width as target)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) The guarantees that the output scaled image has the
 *          dimension(s) you specify.
 *           - To specify the width with isotropic scaling, set @hd = 0.
 *           - To specify the height with isotropic scaling, set @wd = 0.
 *           - If both @wd and @hd are specified, the image is scaled
 *             (in general, anisotropically) to that size.
 *           - It is an error to set both @wd and @hd to 0.
 *}
 function pixScaleToSize( pix: PLPix; wd, hd: Integer): PLPix; cdecl; external LIBLEPT;


  {*------------------------------------------------------------------*
   *                  Scaling by closest pixel sampling               *
   *------------------------------------------------------------------*/
  /*!
   *  pixScaleBySampling()
   *
   *      Input:  pixs (1, 2, 4, 8, 16, 32 bpp)
   *              scalex, scaley
   *      Return: pixd, or null on error
   *
   *  Notes:
   *      (1) This function samples from the source without
   *          filtering.  As a result, aliasing will occur for
   *          subsampling (@scalex and/or @scaley < 1.0).
   *      (2) If @scalex == 1.0 and @scaley == 1.0, returns a copy.
   *}
  function pixScaleBySampling( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;

{*------------------------------------------------------------------*
 *            Fast integer factor subsampling RGB to gray           *
 *------------------------------------------------------------------*/
/*!
 *  pixScaleRGBToGrayFast()
 *
 *      Input:  pixs (32 bpp rgb)
 *              factor (integer reduction factor >= 1)
 *              color (one of COLOR_RED, COLOR_GREEN, COLOR_BLUE)
 *      Return: pixd (8 bpp), or null on error
 *
 *  Notes:
 *      (1) This does simultaneous subsampling by an integer factor and
 *          extraction of the color from the RGB pix.
 *      (2) It is designed for maximum speed, and is used for quickly
 *          generating a downsized grayscale image from a higher resolution
 *          RGB image.  This would typically be used for image analysis.
 *      (3) The standard color byte order (RGBA) is assumed.
 *}
  function pixScaleRGBToGrayFast( pix: PLPix; factor, color: Longint ): PLPix; cdecl; external LIBLEPT;


