{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}


{*
 *  classapp.c                                               PASCAL
 *
 *      Top-level jb2 correlation and rank-hausdorff
 *
 *         l_int32         jbCorrelation()
 *         l_int32         jbRankHaus()
 *
 *      Extract and classify words in textline order
 *
 *         JBCLASSER      *jbWordsInTextlines()
 *         l_int32         pixGetWordsInTextlines()             X
 *         l_int32         pixGetWordBoxesInTextlines()
 *
 *      Use word bounding boxes to compare page images
 *
 *         NUMAA          *boxaExtractSortedPattern()
 *         l_int32         numaaCompareImagesByBoxes()
 *         static l_int32  testLineAlignmentX()
 *         static l_int32  countAlignedMatches()
 *         static void     printRowIndices()
 *}


{*------------------------------------------------------------------*
 *           Extract and classify words in textline order           *
 *------------------------------------------------------------------*}

{*!
 *  pixGetWordsInTextlines()
 *
 *      Input:  pixs (1 bpp, 300 ppi)
 *              reduction (1 for full res; 2 for half-res)
 *              minwidth, minheight (of saved components; smaller are discarded)
 *              maxwidth, maxheight (of saved components; larger are discarded)
 *              &boxad (<return> word boxes sorted in textline line order)
 *              &pixad (<return> word images sorted in textline line order)
 *              &naindex (<return> index of textline for each word)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) The input should be at a resolution of about 300 ppi.
 *          The word masks can be computed at either 150 ppi or 300 ppi.
 *          For the former, set reduction = 2.
 *      (2) The four size constraints on saved components are all
 *          used at 2x reduction.
 *      (3) The result are word images (and their b.b.), extracted in
 *          textline order, all at 2x reduction, and with a numa giving
 *          the textline index for each word.
 *      (4) The pixa and boxa interfaces should make this type of
 *          application simple to put together.  The steps are:
 *           - generate first estimate of word masks
 *           - get b.b. of these, and remove the small and big ones
 *           - extract pixa of the word mask from these boxes
 *           - extract pixa of the actual word images, using word masks
 *           - sort actual word images in textline order (2d)
 *           - flatten them to a pixa (1d), saving the textline index
 *             for each pix
 *      (5) In an actual application, it may be desirable to pre-filter
 *          the input image to remove large components, to extract
 *          single columns of text, and to deskew them.  For example,
 *          to remove both large components and small noisy components
 *          that can interfere with the statistics used to estimate
 *          parameters for segmenting by words, but still retain text lines,
 *          the following image preprocessing can be done:
 *                Pix *pixt = pixMorphSequence(pixs, "c40.1", 0);
 *                Pix *pixf = pixSelectBySize(pixt, 0, 60, 8,
 *                                     L_SELECT_HEIGHT, L_SELECT_IF_LT, NULL);
 *                pixAnd(pixf, pixf, pixs);  // the filtered image
 *          The closing turns text lines into long blobs, but does not
 *          significantly increase their height.  But if there are many
 *          small connected components in a dense texture, this is likely
 *          to generate tall components that will be eliminated in pixf.
 *}
function pixGetWordsInTextlines( pixs: PLPix; reduction, minwidth, minheight, maxwidth, maxheight: Integer;
                       pboxad: PBoxArray; ppixad: PPixArray; pnai: PNumArray): Integer; cdecl; external LIBLEPT;
