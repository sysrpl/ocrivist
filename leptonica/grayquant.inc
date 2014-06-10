{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  grayquant.c                                                           PASCAL
 *
 *      Thresholding from 8 bpp to 1 bpp
 *
 *          Floyd-Steinberg dithering to binary
 *              PIX    *pixDitherToBinary()
 *              PIX    *pixDitherToBinarySpec()
 *
 *          Simple (pixelwise) binarization with fixed threshold
 *              PIX    *pixThresholdToBinary()                                X
 *
 *          Binarization with variable threshold
 *              PIX    *pixVarThresholdToBinary()
 *
 *          Slower implementation of Floyd-Steinberg dithering, using LUTs
 *              PIX    *pixDitherToBinaryLUT()
 *
 *          Generate a binary mask from pixels of particular values
 *              PIX    *pixGenerateMaskByValue()                              X
 *              PIX    *pixGenerateMaskByBand()                               X
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
 *              PIX      *pixGenerateMaskByBand32()                           X
 *              PIX      *pixGenerateMaskByDiscr32()
 *
 *      Histogram-based grayscale quantization
 *              PIX      *pixGrayQuantFromHisto()
 *       static l_int32   numaFillCmapFromHisto()
 *
 *      Color quantize grayscale image using existing colormap
 *              PIX      *pixGrayQuantFromCmap()
 *}

{/*------------------------------------------------------------------*
 *       Simple (pixelwise) binarization with fixed threshold       *
 *------------------------------------------------------------------*/
/*!
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
function pixThresholdToBinary( pixs: PLPix; thresh: Integer): PLPix; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *                 Thresholding from 32 bpp rgb to 1 bpp              *
 *--------------------------------------------------------------------*/
/*!
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
function pixGenerateMaskByBand32( pixs: PLPix; refval: Cardinal; delm, delp: Longint): PLPix; cdecl; external LIBLEPT;

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
function pixGenerateMaskByBand( pixs: PLPix; lower, upper, inband, usecmap: Longint): PLPix; cdecl; external LIBLEPT;

{*--------------------------------------------------------------------*
 *       Generate a binary mask from pixels of particular value(s)    *
 *--------------------------------------------------------------------*/
/*!
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
function pixGenerateMaskByValue( pixs: PLPix; val, usecmap: Longint ): PLPix; cdecl; external LIBLEPT;
