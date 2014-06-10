{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *   pageseg.c                                        PASCAL
 *
 *      Top level page segmentation
 *          l_int32   pixGetRegionsBinary()              X
 *
 *      Halftone region extraction
 *          PIX      *pixGenHalftoneMask()               X
 *
 *      Textline extraction
 *          PIX      *pixGenTextlineMask()               X
 *
 *      Textblock extraction
 *          PIX      *pixGenTextblockMask()              X
 *}

{*------------------------------------------------------------------*
 *                     Top level page segmentation                  *
 *------------------------------------------------------------------*/
/*!
 *  pixGetRegionsBinary()
 *
 *      Input:  pixs (1 bpp, assumed to be 300 to 400 ppi)
 *              &pixhm (<optional return> halftone mask)
 *              &pixtm (<optional return> textline mask)
 *              &pixtb (<optional return> textblock mask)
 *              debug (flag: set to 1 for debug output)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) It is best to deskew the image before segmenting.
 *      (2) The debug flag enables a number of outputs.  These
 *          are included to show how to generate and save/display
 *          these results.
 *}
function pixGetRegionsBinary ( pixs: PLPix; ppixhm, ppixtm, ppixtb: PPLPix; debug: Integer=0): Integer; cdecl; external LIBLEPT;

{/*------------------------------------------------------------------*
 *                    Halftone region extraction                    *
 *------------------------------------------------------------------*/
/*!
 *  pixGenHalftoneMask()
 *
 *      Input:  pixs (1 bpp, assumed to be 150 to 200 ppi)
 *              &pixtext (<optional return> text part of pixs)
 *              &htfound (<optional return> 1 if the mask is not empty)
 *              debug (flag: 1 for debug output)
 *      Return: pixd (halftone mask), or null on error
 *}
function pixGenHalftoneMask( pixs: PLPix; ppixtext: PPLPix; phtfound: PInteger; debug: Integer = 0): PLPix; cdecl; external LIBLEPT;

{*------------------------------------------------------------------*
 *                         Textline extraction                      *
 *------------------------------------------------------------------*}
{*!
 *  pixGenTextlineMask()
 *
 *      Input:  pixs (1 bpp, assumed to be 150 to 200 ppi)
 *              @pixvws (<return> vertical whitespace mask)
 *              @tlfound (<optional return> 1 if the mask is not empty)
 *              debug (flag: 1 for debug output)
 *      Return: pixd (textline mask), or null on error
 *
 *  Notes:
 *      (1) The input pixs should be deskewed.
 *      (2) pixs should have no halftone pixels.
 *      (3) Both the input image and the returned textline mask
 *          are at the same resolution.
 *}

function pixGenTextlineMask ( pixs: PLPix; ppixvws: PPLPix; ptlfound: PLongint; debug: Longint ): PLPix; cdecl; external LIBLEPT;

{*------------------------------------------------------------------*
 *                       Textblock extraction                       *
 *------------------------------------------------------------------*}
{*!
 *  pixGenTextblockMask()
 *
 *      Input:  pixs (1 bpp, textline mask, assumed to be 150 to 200 ppi)
 *              pixvws (vertical white space mask)
 *              debug (flag: 1 for debug output)
 *      Return: pixd (textblock mask), or null on error
 *
 *  Notes:
 *      (1) Both the input masks (textline and vertical white space) and
 *          the returned textblock mask are at the same resolution.
 *      (2) The result is somewhat noisy, in that small "blocks" of
 *          text may be included.  These can be removed by post-processing,
 *          using, e.g.,
 *             pixSelectBySize(pix, 60, 60, 4, L_SELECT_IF_EITHER,
 *                             L_SELECT_IF_GTE, NULL);
 *}

function pixGenTextblockMask( pixs, pixvws: PLPix; debug:Longint ): PLPix; cdecl; external LIBLEPT;

