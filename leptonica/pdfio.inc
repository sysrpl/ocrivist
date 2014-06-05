{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  pdfio.c
 *
 *    |=============================================================|
 *    |                         Important note                      |
 *    |=============================================================|
 *    | Some of these functions require libtiff, libjpeg, and libz  |
 *    | If you do not have these libraries, you must set            |
 *    |      #define  USE_PDFIO     0                               |
 *    | in environ.h.  This will link pdfiostub.c                   |
 *    |=============================================================|
 *
 *     The first set of functions below converts a set of image files
 *     to a multi-page pdf file, with one image on each page.
 *     All images are rendered at the same (input) resolution.
 *     The images can be specified as being in a directory, or they
 *     can be in an sarray.  The output pdf can be either a file
 *     or an array of bytes in memory.
 *
 *     The second set of functions converts a set of images in memory
 *     to a multi-page pdf, with one image on each page.  The pdf
 *     output can be either a file or an array of bytes in memory.
 *
 *     The third set of functions implements a pdf output "device driver"
 *     for wrapping (encoding) any number of images on a single page
 *     in pdf.  The input can be either an image file or a Pix;
 *     the pdf output can be either a file or an array of bytes in memory.
 *
 *     The fourth set of functions (segmented) converts a set of image
 *     files, along with optional segmentation information, and
 *     generates a multi-page pdf file, where each page consists
 *     in general of a mixed raster pdf of image and non-image regions.
 *     The segmentation information for each page can be input as
 *     either a mask over the image parts, or as a Boxa of those
 *     regions.
 *
 *     The fifth set of functions (segmented) converts an image and
 *     an optional Boxa of image regions into a mixed raster pdf file
 *     for the page.  The input image can be either a file or a Pix.
 *
 *     The sixth set of functions takes a set of single-page
 *     pdf files and concatenates them into a multi-page pdf.
 *     The input can be a set of single page pdf files, or of
 *     pdf 'strings' in memory.  The output can be either a file or
 *     an array of bytes in memory.
 *
 *     The images in the pdf file can be rendered using a pdf viewer,
 *     such as gv, evince, xpdf or acroread.
 *
 *     Reference on the pdf file format:
 *         http://www.adobe.com/devnet/pdf/pdf_reference_archive.html
 *
 *     1. Convert specified image files to Pdf (one image file per page)
 *          l_int32             convertFilesToPdf()
 *          l_int32             saConvertFilesToPdf()
 *          l_int32             saConvertFilesToPdfData()
 *          l_int32             selectDefaultPdfEncoding()
 *
 *     2. Convert multiple images to Pdf (one image per page)
 *          l_int32             pixaConvertToPdf()
 *          l_int32             pixaConvertToPdfData()
 *
 *     3. Single page, multi-image converters
 *          l_int32             convertToPdf()
 *          l_int32             convertImageDataToPdf()
 *          l_int32             convertToPdfData()
 *          l_int32             convertImageDataToPdfData()
 *          l_int32             pixConvertToPdf()
 *          l_int32             pixConvertToPdfData()
 *          l_int32             pixWriteStreamPdf()
 *
 *     4. Segmented multi-page, multi-image converter
 *          l_int32             convertSegmentedFilesToPdf()
 *          BOXAA              *convertNumberedMasksToBoxaa()
 *
 *     5. Segmented single page, multi-image converters
 *          l_int32             convertToPdfSegmented()
 *          l_int32             pixConvertToPdfSegmented()
 *          l_int32             convertToPdfDataSegmented()
 *          l_int32             pixConvertToPdfDataSegmented()
 *
 *     Helper functions for generating the output pdf string
 *          static l_int32      l_generatePdf()
 *          static void         generateFixedStringsPdf()
 *          static void         generateMediaboxPdf()
 *          static l_int32      generatePageStringPdf()
 *          static l_int32      generateContentStringPdf()
 *          static l_int32      generatePreXStringsPdf()
 *          static l_int32      generateColormapStringsPdf()
 *          static void         generateTrailerPdf()
 *          static l_int32      makeTrailerStringPdf()
 *          static l_int32      generateOutputDataPdf()
 *
 *     6. Multi-page concatenation
 *          l_int32             concatenatePdf()
 *          l_int32             saConcatenatePdf()
 *          l_int32             ptraConcatenatePdf()
 *          l_int32             concatenatePdfToData()
 *          l_int32             saConcatenatePdfToData()
 *          l_int32             ptraConcatenatePdfToData()
 *
 *     Helper functions for generating the multi-page pdf output
 *          static l_int32      parseTrailerPdf()
 *          static char        *generatePagesObjStringPdf()
 *          static L_BYTEA     *substituteObjectNumbers()
 *
 *     Create/destroy/access pdf data
 *          static L_PDF_DATA         *pdfdataCreate()
 *          static void                pdfdataDestroy()
 *          static L_COMPRESSED_DATA  *pdfdataGetCid()
 *
 *     Set flags for special modes
 *          void                l_pdfSetG4ImageMask()
 *          void                l_pdfSetDateAndVersion()
 *
 *     The top-level multi-image functions can be visualized as follows:
 *          Output pdf data to file:
 *             convertToPdf()  and  convertImageDataToPdf()
 *                     --> pixConvertToPdf()
 *                           --> pixConvertToPdfData()
 *
 *          Output pdf data to array in memory:
 *             convertToPdfData()  and  convertImageDataToPdfData()
 *                     --> pixConvertToPdfData()
 *
 *     The top-level segmented image functions can be visualized as follows:
 *          Output pdf data to file:
 *             convertToPdfSegmented()
 *                     --> pixConvertToPdfSegmented()
 *                           --> pixConvertToPdfDataSegmented()
 *
 *          Output pdf data to array in memory:
 *             convertToPdfDataSegmented()
 *                     --> pixConvertToPdfDataSegmented()
 *
 *     For multi-page concatenation, there are three different types of input
 *        (1) directory and optional filename filter
 *        (2) sarray of filenames
 *        (3) ptra of byte arrays of pdf data
 *     and two types of output for the concatenated pdf data
 *        (1) filename
 *        (2) data array and size
 *     High-level interfaces are given for each of the six combinations.
 *}

const
        L_JPEG_ENCODE  = 1;
        L_G4_ENCODE    = 2;
        L_FLATE_ENCODE = 3;

 {*
  *  saConvertFilesToPdf()
  *
  *      Input:  sarray (of pathnames for images)
  *              res (input resolution of all images)
  *              scalefactor (scaling factor applied to each image)
  *              type (encoding type (L_JPEG_ENCODE, L_G4_ENCODE,
  *                    L_FLATE_ENCODE, or 0 for default)
  *              quality (used for JPEG only; 0 for default (75))
  *              title (<optional> pdf title; if null, taken from the first
  *                     image filename)
  *              fileout (pdf file of all images)
  *      Return: 0 if OK, 1 on error
  *
  *  Notes:
  *      (1) The images are encoded with G4 if 1 bpp; JPEG if 8 bpp without
  *          colormap and many colors, or 32 bpp; FLATE for anything else.
  *}
function saConvertFilesToPdf 	( sa: PSArray; res: longint; scalefactor: Single;
                                        encoding, quality: longint; title, fileout: PChar): Longint; cdecl; external LIBLEPT;

