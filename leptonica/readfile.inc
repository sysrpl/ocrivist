{
 * -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *
 *  readfile.c:  reads image on file into memory
 *
 *      Top-level functions for reading images from file       INCLUDED
 *           PIXA      *pixaReadFiles()
 *           PIXA      *pixaReadFilesSA()
 *           PIX       *pixRead()                                 X
 *           PIX       *pixReadWithHint()
 *           PIX       *pixReadIndexed()
 *           PIX       *pixReadStream()
 *
 *      Read header information from file
 *           l_int32    pixReadHeader()                           X
 *
 *      Format finders
 *           l_int32    findFileFormat()                          X
 *           l_int32    findFileFormatStream()
 *           l_int32    findFileFormatBuffer()                    X
 *           l_int32    fileFormatIsTiff()
 *
 *      Read from memory
 *           PIX       *pixReadMem()                              X
 *           l_int32    pixReadHeaderMem()                        X
 *
 *      Test function for I/O with different formats
 *           l_int32    ioFormatTest()
 *}



{*!
 *  pixRead()
 *
 *      Input:  filename (with full pathname or in local directory)
 *      Return: pix if OK; null on error
 *}
function pixRead ( filename: PChar ): PLPix; cdecl; external LIBLEPT;

{ *  pixReadHeader()
  *
  *      Input:  filename (with full pathname or in local directory)
  *              &format (<optional return> file format)
  *              &w, &h (<optional returns> width and height)
  *              &bps <optional return> bits/sample
  *              &spp <optional return> samples/pixel (1, 3 or 4)
  *              &iscmap (<optional return> 1 if cmap exists; 0 otherwise)
  *      Return: 0 if OK, 1 on error }
function pixReadHeader( filename: PChar; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer; cdecl; external LIBLEPT;

{*  pixReadMem()
 *
 *      Input:  data (const; encoded)
 *              datasize (size of data)
 *      Return: pix, or null on error
 *
 *  Notes:
 *      (1) This is a variation of pixReadStream(), where the data is read
 *          from a memory buffer rather than a file.
 *      (2) On windows, this will only read tiff formatted files from
 *          memory.  For other formats, it requires fmemopen(3).
 *          Attempts to read those formats will fail at runtime.
 *      (3) findFileFormatBuffer() requires up to 8 bytes to decide on
 *          the format.  That determines the constraint here.}
function pixReadMem(data: PByte; size: Cardinal): PLPix; cdecl; external LIBLEPT;

{ *  pixReadHeaderMem()
 *
 *      Input:  data (const; encoded)
 *              datasize (size of data)
 *              &format (<optional return> file format)
 *              &w, &h (<optional returns> width and height)
 *              &bps <optional return> bits/sample
 *              &spp <optional return> samples/pixel (1, 3 or 4)
 *              &iscmap (<optional return> 1 if cmap exists; 0 otherwise)
 *      Return: 0 if OK, 1 on error }
function pixReadHeaderMem( data: PByte; size: Cardinal; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer; cdecl; external LIBLEPT;


{*---------------------------------------------------------------------*
 *                            Format finders                           *
 *---------------------------------------------------------------------*}

{*  findFileFormat()
 *
 *      Input:  filename
 *              &format (<return>)
 *      Return: 0 if OK, 1 on error or if format is not recognized }
  function findFileFormat( filename: PChar; pformat: PInteger): Integer; cdecl; external LIBLEPT;

{*  findFileFormatBuffer()
 *
 *      Input:  byte buffer (at least 12 bytes in size; we can't check)
 *              &format (<return>)
 *      Return: 0 if OK, 1 on error or if format is not recognized
 *
 *  Notes:
 *      (1) This determines the file format from the first 12 bytes in
 *          the compressed data stream, which are stored in memory.
 *      (2) For tiff files, this returns IFF_TIFF.  The specific tiff
 *          compression is then determined using findTiffCompression().}
  function findFileFormatBuffer(buf: PByte ; pformat: PInteger): Integer; cdecl; external LIBLEPT;
