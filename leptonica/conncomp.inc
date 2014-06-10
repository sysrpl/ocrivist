{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  conncomp.c
 *
 *    Connected component counting and extraction, using Heckbert's
 *    stack-based filling algorithm.
 *
 *      4- and 8-connected components: counts, bounding boxes and images
 *                                                         PASCAL
 *      Top-level calls:
 *           BOXA     *pixConnComp()                          X
 *           BOXA     *pixConnCompPixa()
 *           BOXA     *pixConnCompBB()                        X
 *           l_int32   pixCountConnComp()
 *
 *      Identify the next c.c. to be erased:
 *           l_int32   nextOnPixelInRaster()
 *           l_int32   nextOnPixelInRasterLow()
 *
 *      Erase the c.c., saving the b.b.:
 *           BOX      *pixSeedfillBB()
 *           BOX      *pixSeedfill4BB()
 *           BOX      *pixSeedfill8BB()
 *
 *      Just erase the c.c.:
 *           l_int32   pixSeedfill()
 *           l_int32   pixSeedfill4()
 *           l_int32   pixSeedfill8()
 *
 *      Static stack helper functions for single raster line seedfill:
 *           static void    pushFillsegBB()
 *           static void    pushFillseg()
 *           static void    popFillseg()
 *
 *  The basic method in pixConnCompBB() is very simple.  We scan the
 *  image in raster order, looking for the next ON pixel.  When it
 *  is found, we erase it and every pixel of the 4- or 8-connected
 *  component to which it belongs, using Heckbert's seedfill
 *  algorithm.  As pixels are erased, we keep track of the
 *  minimum rectangle that encloses all erased pixels; after
 *  the connected component has been erased, we save its
 *  bounding box in an array of boxes.  When all pixels in the
 *  image have been erased, we have an array that describes every
 *  4- or 8-connected component in terms of its bounding box.
 *
 *  pixConnCompPixa() is a slight variation on pixConnCompBB(),
 *  where we additionally save an array of images (in a Pixa)
 *  of each of the 4- or 8-connected components.  This is done trivially
 *  by maintaining two temporary images.  We erase a component from one,
 *  and use the bounding box to extract the pixels within the b.b.
 *  from each of the two images.  An XOR between these subimages
 *  gives the erased component.  Then we erase the component from the
 *  second image using the XOR again, with the extracted component
 *  placed on the second image at the location of the bounding box.
 *  Rasterop does all the work.  At the end, we have an array
 *  of the 4- or 8-connected components, as well as an array of the
 *  bounding boxes that describe where they came from in the original image.
 *
 *  If you just want the number of connected components, pixCountConnComp()
 *  is a bit faster than pixConnCompBB(), because it doesn't have to
 *  keep track of the bounding rectangles for each c.c.
 *}

{*-----------------------------------------------------------------------*
 *                Bounding boxes of 4 Connected Components               *
 *-----------------------------------------------------------------------*/
/*!
 *  pixConnComp()
 *
 *      Input:  pixs (1 bpp)
 *              &pixa   (<optional return> pixa of each c.c.)
 *              connectivity (4 or 8)
 *      Return: boxa, or null on error
 *
 *  Notes:
 *      (1) This is the top-level call for getting bounding boxes or
 *          a pixa of the components, and it can be used instead
 *          of either pixConnCompBB() or pixConnCompPixa(), rsp.
 *}
function pixConnComp( pixs: PLPix; ppixa: PPixArray; connectivity: Integer ): PBoxArray; cdecl; external LIBLEPT;

{*!
 *  pixConnCompBB()
 *
 *      Input:  pixs (1 bpp)
 *              connectivity (4 or 8)
 *      Return: boxa, or null on error
 *
 * Notes:
 *     (1) Finds bounding boxes of 4- or 8-connected components
 *         in a binary image.
 *     (2) This works on a copy of the input pix.  The c.c. are located
 *         in raster order and erased one at a time.  In the process,
 *         the b.b. is computed and saved.
 *}
function pixConnCompBB( pixs: PLPix; connectivity: Integer ): PBoxArray; cdecl; external LIBLEPT;


