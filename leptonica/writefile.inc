{*===================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 * writefile.c
 *
 *     High-level procedures for writing images to file:
 *        l_int32     pixaWriteFiles()
 *        l_int32     pixWrite()                                 X
 *        l_int32     pixWriteStream()
 *        l_int32     pixWriteImpliedFormat()
 *        l_int32     pixWriteTempfile()
 *
 *     Selection of output format if default is requested
 *        l_int32     pixChooseOutputFormat()
 *        l_int32     getImpliedFileFormat()
 *        const char *getFormatExtension()
 *
 *     Write to memory
 *        l_int32     pixWriteMem()
 *                                                               X
 *     Image display for debugging
 *        l_int32     pixDisplay()
 *        l_int32     pixDisplayWithTitle()
 *        l_int32     pixDisplayMultiple()
 *        l_int32     pixDisplayWrite()
 *        l_int32     pixDisplayWriteFormat()
 *        l_int32     pixSaveTiled()
 *        l_int32     pixSaveTiledOutline()
 *        l_int32     pixSaveTiledWithText()
 *        void        l_chooseDisplayProg()
 *}

 {*---------------------------------------------------------------------*
  *           Top-level procedures for writing images to file           *
  *---------------------------------------------------------------------*}

{*!
 *  pixWrite()
 *
 *      Input:  filename
 *              pix
 *              format  (defined in imageio.h)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Open for write using binary mode (with the "b" flag)
 *          to avoid having Windows automatically translate the NL
 *          into CRLF, which corrupts image files.  On non-windows
 *          systems this flag should be ignored, per ISO C90.
 *          Thanks to Dave Bryan for pointing this out.
 *      (2) If the default image format is requested, we use the input format;
 *          if the input format is unknown, a lossless format is assigned.
 *      (3) There are two modes with respect to file naming.
 *          (a) The default code writes to @filename.
 *          (b) If WRITE_AS_NAMED is defined to 0, it's a bit fancier.
 *              Then, if @filename does not have a file extension, one is
 *              automatically appended, depending on the requested format.
 *          The original intent for providing option (b) was to insure
 *          that filenames on Windows have an extension that matches
 *          the image compression.  However, this is not the default.
 *}
function pixWrite(filename: PChar; pix: PLPix; format: Longint): Longint;  cdecl; external LIBLEPT;

{*---------------------------------------------------------------------*
 *                            Write to memory                          *
 *---------------------------------------------------------------------*}
{*!
 *  pixWriteMem()
 *
 *      Input:  @data (<return> data of tiff compressed image)
 *              @size (<return> size of returned data)
 *              pix
 *              format  (defined in imageio.h)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) On windows, this will only write tiff and PostScript to memory.
 *          For other formats, it requires open_memstream(3).
 *      (2) PostScript output is uncompressed, in hex ascii.
 *          Most printers support level 2 compression (tiff_g4 for 1 bpp,
 *          jpeg for 8 and 32 bpp).
 *}
function pixWriteMem(pdata: Pointer{array of byte}; psize: PInteger; pix: PLPix; imageformat: Integer): Integer; cdecl; external LIBLEPT;


