{*====================================================================*
   Leptonica.pas provides Pascal bindings for liblept
   Author: Malcolm Poole <mgpoole@users.sourceforge.net> 2011
 *====================================================================*}

{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 -  This software is distributed in the hope that it will be
 -  useful, but with NO WARRANTY OF ANY KIND.
 -  No author or distributor accepts responsibility to anyone for the
 -  consequences of using this software, or for whether it serves any
 -  particular purpose or works at all, unless he or she says so in
 -  writing.  Everyone is granted permission to copy, modify and
 -  redistribute this source code, for commercial or non-commercial
 -  purposes, with the following restrictions: (1) the origin of this
 -  source code must not be misrepresented; (2) modified versions must
 -  be plainly marked as such; and (3) this notice may not be removed
 -  or altered from any source or modified source distribution.
 *====================================================================*}

unit leptonica;

{$mode objfpc}{$H+}

interface

const
  LIBLEPT = 'lept';

  ImageFileFormatExtensions: array[0..18] of string = ( 'unknown',
          'bmp',
          'jpg',
          'png',
          'tif',
          'tif',
          'tif',
          'tif',
          'tif',
          'tif',
          'tif',
          'pnm',
          'ps',
          'gif',
          'jp2',
          'webp',
          'pdf',
          'default',
          '' );

      IFF_UNKNOWN        = 0;
      IFF_BMP            = 1;
      IFF_JFIF_JPEG      = 2;
      IFF_PNG            = 3;
      IFF_TIFF           = 4;
      IFF_TIFF_PACKBITS  = 5;
      IFF_TIFF_RLE       = 6;
      IFF_TIFF_G3        = 7;
      IFF_TIFF_G4        = 8;
      IFF_TIFF_LZW       = 9;
      IFF_TIFF_ZIP       = 10;
      IFF_PNM            = 11;
      IFF_PS             = 12;
      IFF_GIF            = 13;
      IFF_JP2            = 14;
      IFF_WEBP           = 15;
      IFF_LPDF           = 16;
      IFF_DEFAULT        = 17;
      IFF_SPIX           = 18;

type

  PLPix = ^TLPix;
  TLPix = record
     w:             Integer;           // width in pixels
     h:             Integer;           // height in pixels
     d:             Integer;           // depth in bits
     wpl:           Cardinal;          // 32-bit words/line
     refcount:      Cardinal;          // reference count (1 if no clones)
     xres:          Integer;           // image res (ppi) in x direction
                                       // (use 0 if unknown)
     yres:          Integer;           // image res (ppi) in y direction
                                       // (use 0 if unknown)
     informat:      Integer;           // input file format, IFF_*
     text:          PChar;             // text string associated with pix
     colormap:      Pointer;           // to colormap (may be null)
     data:          PCardinal;         // the image data
  end;

  PPLPix = ^PLPix;

  PPLBox = ^PLBox;
  PLBox = ^TLBox;
  TLBox = record
    x:         Longint;
    y:         Longint;
    w:         Longint;
    h:         Longint;
    refcount:  Cardinal; //reference count (1 if no clones)
  end;

 PBoxArray = ^TBoxArray;
 TBoxArray = record
    n:        integer;             // number of box in ptr array
    nalloc:   integer;             // number of box ptrs allocated
    refcount: Cardinal;            // reference count (1 if no clones)
    box:      array of PLBox;      // box ptr array
  end;

 PPBoxArrayArray = ^PBoxArrayArray;
 PBoxArrayArray = ^TBoxArrayArray;
 TBoxArrayArray = record
    n:        integer;             // number of box in ptr array
    nalloc:   integer;             // number of box ptrs allocated
    boxa:      array of PBoxArray; // boxa ptr array
  end;


  PPixArray = ^TPixArray;
  TPixArray = record
    n:       Integer;            // number of Pix in ptr array
    nalloc:  Integer;            // number of Pix ptrs allocated
    refcount:Cardinal;            // reference count (1 if no clones)
    pix:     array of PLPix;           // the array of ptrs to pix
    boxa:    array of TLBox;          // array of boxes
  end;

  PPointArray = ^TPointArray;
  TPointArray = record
    n: Integer;             //* actual number of pts
    nalloc: Integer;        //* size of allocated arrays
    refcount: Integer;      //* reference count (1 if no clones)
    x, y: Array of Single;  //* arrays of floats
  end;


  PNumArray = ^TNumArray;
  TNumArray = record
    nalloc:         Integer;           //* size of allocated number array
    n:              Integer;           //* number of numbers saved
    refcount:       Integer;           //* reference count (1 if no clones)
    startx:         Single;            //* x value assigned to array[0]
    delx:           Single;            //* change in x value as i --> i + 1
    numarray:       array of Single;   //* number array
  end;

  PPta = ^TPta;
  TPta = record
     n:          Integer;             // actual number of pts
     nalloc:     Integer;             // size of allocated arrays
     refcount:   Integer;             // reference count (1 if no clones)
     x, y:       Single;              // arrays of floats
  end;

  PPtaArray = ^TPtaArray;
  TPtaArray = record
     n:          Integer;           // number of pta in ptr array
     nalloc:     Integer;           // number of pta ptrs allocated
     pta:        array of PPta;     // pta ptr array
  end;

{*-------------------------------------------------------------------------*
 *                         Access and storage flags                        *
 *-------------------------------------------------------------------------*/
/*
 *  For Pix, Box, Pta and Numa, there are 3 standard methods for handling
 *  the retrieval or insertion of a struct:
 *     (1) direct insertion (Don't do this if there is another handle
 *                           somewhere to this same struct!)
 *     (2) copy (Always safe, sets up a refcount of 1 on the new object.
 *               Can be undesirable if very large, such as an image or
 *               an array of images.)
 *     (3) clone (Makes another handle to the same struct, and bumps the
 *                refcount up by 1.  Safe to do unless you're changing
 *                data through one of the handles but don't want those
 *                changes to be seen by the other handle.)
 *
 *  For Pixa and Boxa, which are structs that hold an array of clonable
 *  structs, there is an additional method:
 *     (4) copy-clone (Makes a new higher-level struct with a refcount
 *                     of 1, but clones all the structs in the array.)
 *
 *  Unlike the other structs, when retrieving a string from an Sarray,
 *  you are allowed to get a handle without a copy or clone (i.e., that
 *  you don't own!).  You must not free or insert such a string!
 *  Specifically, for an Sarray, the copyflag for retrieval is either:
 *         TRUE (or 1 or L_COPY)
 *  or
 *         FALSE (or 0 or L_NOCOPY)
 *  For insertion, the copyflag is either:
 *         TRUE (or 1 or L_COPY)
 *  or
 *         FALSE (or 0 or L_INSERT)
 *  Note that L_COPY is always 1, and L_INSERT and L_NOCOPY are always 0.
 *}
const
    L_INSERT = 0;     // stuff it in; no copy, clone or copy-clone
    L_COPY = 1;       // make/use a copy of the object
    L_CLONE = 2;      // make/use clone (ref count) of the object
    L_COPY_CLONE = 3; // make a new object and fill with with clones
                      // of each object in the array(s)

const
    L_NOCOPY = 0;     // copyflag value in sarrayGetString()

  function pixRead ( filename: PChar ): PLPix; cdecl; external LIBLEPT;
  function pixCreate( w, h, format: Integer ): TLPix; cdecl; external LIBLEPT;
  function pixClone ( pixs: PLPix ): PLPix; cdecl; external LIBLEPT;
  procedure pixDestroy ( pix: PLPix ); cdecl; external LIBLEPT;
  procedure numaDestroy ( pna: PNumArray ); cdecl; external LIBLEPT;
  procedure ptaDestroy ( pta: PPointArray ); cdecl; external LIBLEPT;
  function pixGetInputFormat ( Pix: TLPix ): Integer; cdecl; external LIBLEPT;
  function pixGetXRes ( Pix: PLPix ): Integer; cdecl; external LIBLEPT;
  function pixGetYRes ( Pix: PLPix ): Integer; cdecl; external LIBLEPT;
  function pixSetResolution ( pix: PLPix; xres, yres: Integer ): Integer; cdecl; external LIBLEPT;
  function pixWriteStream( fp: Pointer; pix: PLPix; imagefileformat: Integer): Integer; cdecl; external LIBLEPT;
  function pixRotate90 (pixs: PLPix; rotatedirection: Integer ): PLPix; cdecl; external LIBLEPT;
  function pixSobelEdgeFilter ( pixs: PLPix; orientflag: Integer ): PLPix;  cdecl; external LIBLEPT;
  function ptaWriteStream ( fp: pointer; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;
  function ptaWrite (  filename: PChar; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;
  function pixGetDimensions( pix: PLPix; pw, ph, pd: PInteger): Integer; cdecl; external LIBLEPT;
  function pixScale( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;
  function pixScaleSmooth( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;
  function pixGetDepth( pix: PLPix): Integer; cdecl; external LIBLEPT;
  function boxaaCreate(n: Integer):PBoxArrayArray; cdecl; external LIBLEPT;
  procedure boxaaDestroy(pbaa: PPBoxArrayArray); cdecl; external LIBLEPT;

 {
  function pixScaleToSize( pix: PLPix; wd, hd: Integer): PLPix;
  function pixScaleBySampling( pix: PLPix; pw, ph: Single): PLPix;
  function pixScaleRGBToGrayFast( pix: PLPix; pw, ph: Single): PLPix;
  function pixReadHeader( filename: PChar; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer;
  function findFileFormat( filename: PChar; pformat: PInteger): Integer;
  function findFileFormatBuffer(buf: PByte ; pformat: PInteger): Integer;
  function pixReadMem(data: PByte; size: PCardinal): PLPix;
  function pixReadHeaderMem( data: PByte; size: PCardinal; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer;
  function pixDeskew( pixs: PLPix; redsearch: Integer ): PLPix;
  function pixDeskewLocal( pixs: PLPix; nslices, redsweep, redsearch: Integer; sweeprange, sweepdelta, minbsdelta: Single): PLPix;
  function pixDeskewGeneral( pixs: PLPix; redsweep: integer; sweeprange, sweepdelta: Single;
                           redsearch, thresh: Integer; pangle, pconf: PSingle ): PLPix;
 }


 {*!
  *  pixGetText()
  *
  *      Input:  pix
  *      Return: ptr to existing text string
  *
  *  Notes:
  *      (1) The text string belongs to the pix.  The caller must
  *          NOT free it!
  *}
 function pixGetText( pix: PLPix ): PChar; cdecl; external LIBLEPT;


{*!
 *  pixSetText()
 *
 *      Input:  pix
 *              textstring (can be null)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This removes any existing textstring and puts a copy of
 *          the input textstring there.
 *}
function pixSetText( pix: PLPix; textstring: PChar ): Integer; cdecl; external LIBLEPT;

{*!
 *  pixAddText()
 *
 *      Input:  pix
 *              textstring
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This adds the new textstring to any existing text.
 *      (2) Either or both the existing text and the new text
 *          string can be null.
 *}
function pixAddText( pix: PLPix; textstring: PChar ): Integer; cdecl; external LIBLEPT;



 {*!
  *  pixRotate90()
  *
  *      Input:  pixs (all depths)
  *              direction (1 = clockwise,  -1 = counter-clockwise)
  *      Return: pixd, or null on error
  *
  *  Notes:
  *      (1) This does a 90 degree rotation of the image about the center,
  *          either cw or ccw, returning a new pix.
  *      (2) The direction must be either 1 (cw) or -1 (ccw).
  *}
 function pixRotate90 	( pixs: TLPix; direction: Integer ): TLPix; cdecl; external LIBLEPT;

 {*!
 *  pixRotate180()
 *
 *      Input:  pixd  (<optional>; can be null, equal to pixs,
 *                     or different from pixs)
 *              pixs (all depths)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does a 180 rotation of the image about the center,
 *          which is equivalent to a left-right flip about a vertical
 *          line through the image center, followed by a top-bottom
 *          flip about a horizontal line through the image center.
 *      (2) There are 3 cases for input:
 *          (a) pixd == null (creates a new pixd)
 *          (b) pixd == pixs (in-place operation)
 *          (c) pixd != pixs (existing pixd)
 *      (3) For clarity, use these three patterns, respectively:
 *          (a) pixd = pixRotate180(NULL, pixs);
 *          (b) pixRotate180(pixs, pixs);
 *          (c) pixRotate180(pixd, pixs);
 *}
function pixRotate180( pixd, pixs: PLPix): PLPix; cdecl; external LIBLEPT;


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
  function pixScaleRGBToGrayFast( pix: PLPix; pw, ph: Single): PLPix; cdecl; external LIBLEPT;



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

  {/*---------------------------------------------------------------------*
 *               Projective transform to remove local skew             *
 *---------------------------------------------------------------------*/
/*!
 *  pixDeskewLocal()
 *
 *      Input:  pixs
 *              nslices  (the number of horizontal overlapping slices; must
 *                  be larger than 1 and not exceed 20; use 0 for default)
 *              redsweep (sweep reduction factor: 1, 2, 4 or 8;
 *                        use 0 for default value)
 *              redsearch (search reduction factor: 1, 2, 4 or 8, and
 *                         not larger than redsweep; use 0 for default value)
 *              sweeprange (half the full range, assumed about 0; in degrees;
 *                          use 0.0 for default value)
 *              sweepdelta (angle increment of sweep; in degrees;
 *                          use 0.0 for default value)
 *              minbsdelta (min binary search increment angle; in degrees;
 *                          use 0.0 for default value)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This function allows deskew of a page whose skew changes
 *          approximately linearly with vertical position.  It uses
 *          a projective tranform that in effect does a differential
 *          shear about the LHS of the page, and makes all text lines
 *          horizontal.
 *      (2) The origin of the keystoning can be either a cheap document
 *          feeder that rotates the page as it is passed through, or a
 *          camera image taken from either the left or right side
 *          of the vertical.
 *      (3) The image transformation is a projective warping,
 *          not a rotation.  Apart from this function, the text lines
 *          must be properly aligned vertically with respect to each
 *          other.  This can be done by pre-processing the page; e.g.,
 *          by rotating or horizontally shearing it.
 *          Typically, this can be achieved by vertically aligning
 *          the page edge.}
  function pixDeskewLocal( pixs: PLPix; nslices, redsweep, redsearch: Integer; sweeprange, sweepdelta, minbsdelta: Single): PLPix; cdecl; external LIBLEPT;

{*---------------------------------------------------------------------*
 *                    Locate text baselines in an image                *
 *---------------------------------------------------------------------*}
{*!
 *  pixFindBaselines()
 *
 *      Input:  pixs (1 bpp)
 *              &pta (<optional return> pairs of pts corresponding to
 *                    approx. ends of each text line)
 *              debug (usually 0; set to 1 for debugging output)
 *      Return: na (of baseline y values), or null on error
 *
 *  Notes:
 *      (1) Input binary image must have text lines already aligned
 *          horizontally.  This can be done by either rotating the
 *          image with pixDeskew(), or, if a projective transform
 *          is required, by doing pixDeskewLocal() first.
 *      (2) Input null for &pta if you don't want this returned.
 *          The pta will come in pairs of points (left and right end
 *          of each baseline).
 *      (3) Caution: this will not work properly on text with multiple
 *          columns, where the lines are not aligned between columns.
 *          If there are multiple columns, they should be extracted
 *          separately before finding the baselines.
 *      (4) This function constructs different types of output
 *          for baselines; namely, a set of raster line values and
 *          a set of end points of each baseline.
 *      (5) This function was designed to handle short and long text lines
 *          without using dangerous thresholds on the peak heights.  It does
 *          this by combining the differential signal with a morphological
 *          analysis of the locations of the text lines.  One can also
 *          combine this data to normalize the peak heights, by weighting
 *          the differential signal in the region of each baseline
 *          by the inverse of the width of the text line found there.
 *      (6) There are various debug sections that can be turned on
 *          with the debug flag.
 }
   function pixFindBaselines( pixs: PLPix; ppta: PPointArray; debug: Integer = 0): PNumArray; cdecl; external LIBLEPT;

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

{/*------------------------------------------------------------------*
 *                         Textline extraction                      *
 *------------------------------------------------------------------*/
/*!
 *  pixGenTextlineMask()
 *
 *      Input:  pixs (1 bpp, assumed to be 150 to 200 ppi)
 *              &pixvws (<return> vertical whitespace mask)
 *              &tlfound (<optional return> 1 if the mask is not empty)
 *              debug (flag: 1 for debug output)
 *      Return: pixd (textline mask), or null on error
 *
 *  Notes:
 *      (1) The input pixs should be deskewed.
 *      (2) pixs should have no halftone pixels.
 *      (3) Both the input image and the returned textline mask
 *          are at the same resolution.
 *}
function pixGenTextlineMask( pixs: PLPix; ppixvws: PPLPix; ptlfound: PInteger; debug: Integer = 0): PLPix; cdecl; external LIBLEPT;

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
function pixWrite( filename: PChar; pix: PLPix; format: Integer): Integer; cdecl; external LIBLEPT;

{*---------------------------------------------------------------------*
 *                            Write to memory                          *
 *---------------------------------------------------------------------*/
/*!
 *  pixWriteMem()
 *
 *      Input:  &data (<return> data of tiff compressed image)
 *              &size (<return> size of returned data)
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

{*-------------------------------------------------------------*
 *                Extract rectangular region                   *
 *-------------------------------------------------------------*/
/*!
 *  pixClipRectangle()
 *
 *      Input:  pixs
 *              box  (requested clipping region; const)
 *              &boxc (<optional return> actual box of clipped region)
 *      Return: clipped pix, or null on error or if rectangle
 *              doesn't intersect pixs
 *
 *  Notes:
 *
 *  This should be simple, but there are choices to be made.
 *  The box is defined relative to the pix coordinates.  However,
 *  if the box is not contained within the pix, we have two choices:
 *
 *      (1) clip the box to the pix
 *      (2) make a new pix equal to the full box dimensions,
 *          but let rasterop do the clipping and positioning
 *          of the src with respect to the dest
 *
 *  Choice (2) immediately brings up the problem of what pixel values
 *  to use that were not taken from the src.  For example, on a grayscale
 *  image, do you want the pixels not taken from the src to be black
 *  or white or something else?  To implement choice 2, one needs to
 *  specify the color of these extra pixels.
 *
 *  So we adopt (1), and clip the box first, if necessary,
 *  before making the dest pix and doing the rasterop.  But there
 *  is another issue to consider.  If you want to paste the
 *  clipped pix back into pixs, it must be properly aligned, and
 *  it is necessary to use the clipped box for alignment.
 *  Accordingly, this function has a third (optional) argument, which is
 *  the input box clipped to the src pix.
 *}
function pixClipRectangle( pixs: PLPix; box: PLBox; pboxc: PPLBox): PLPix; cdecl; external LIBLEPT;

{*---------------------------------------------------------------------*
 *                  Box creation, destruction and copy                 *
 *---------------------------------------------------------------------*/
/*!
 *  boxCreate()
 *
 *      Input:  x, y, w, h
 *      Return: box, or null on error
 *
 *  Notes:
 *      (1) This clips the box to the +quad.  If no part of the
 *          box is in the +quad, this returns NULL.
 *      (2) We allow you to make a box with w = 0 and/or h = 0.
 *          This does not represent a valid region, but it is useful
 *          as a placeholder in a boxa for which the index of the
 *          box in the boxa is important.  This is an atypical
 *          situation; usually you want to put only valid boxes with
 *          nonzero width and height in a boxa.  If you have a boxa
 *          with invalid boxes, the accessor boxaGetValidBox()
 *          will return NULL on each invalid box.
 *      (3) If you want to create only valid boxes, use boxCreateValid(),
 *          which returns NULL if either w or h is 0.
 *}
function boxCreate(x, y, w, h: longint): PLBox; cdecl; external LIBLEPT;

{*!
 *  boxCreateValid()
 *
 *      Input:  x, y, w, h
 *      Return: box, or null on error
 *
 *  Notes:
 *      (1) This returns NULL if either w = 0 or h = 0.
 *}
function boxCreateValid(x, y, w, h: longint): PLBox; cdecl; external LIBLEPT;

{*!
 *  boxDestroy()
 *
 *      Input:  &box (<will be set to null before returning>)
 *      Return: void
 *
 *  Notes:
 *      (1) Decrements the ref count and, if 0, destroys the box.
 *      (2) Always nulls the input ptr.
 *}
procedure boxDestroy( pbox: PLBox ); cdecl; external LIBLEPT;

{*!
 *  boxGetGeometry()
 *
 *      Input:  box
 *              &x, &y, &w, &h (<optional return>; each can be null)
 *      Return: 0 if OK, 1 on error
 *}
function boxGetGeometry( box: PLBox; px, py, pw, ph: PLongint): LongInt; cdecl; external LIBLEPT;

{*!
 *  boxSetGeometry()
 *
 *      Input:  box
 *              x, y, w, h (use -1 to leave unchanged)
 *      Return: 0 if OK, 1 on error
 *}
 function boxSetGeometry( box: PLBox; px, py, pw, ph: Longint): LongInt; cdecl; external LIBLEPT;


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

function pixConnCompBB( pixs: PLPix; connectivity: Integer ): PBoxArray; cdecl; external LIBLEPT;

{*!
 *  boxaWrite()
 *
 *      Input:  filename
 *              boxa
 *      Return: 0 if OK, 1 on error
 *}
function boxaWrite(filename: Pchar; boxa: PBoxArray): Integer; cdecl; external LIBLEPT;

function boxaGetCount( boxa: PBoxArray): Integer; cdecl; external LIBLEPT;

procedure boxaDestroy( pboxa: PBoxArray); cdecl; external LIBLEPT;

{*!
 *  boxaGetBox()
 *
 *      Input:  boxa
 *              index  (to the index-th box)
 *              accessflag  (L_COPY or L_CLONE)
 *      Return: box, or null on error
 *}
function boxaGetBox( boxa: PBoxArray; index: Integer; accessflag: Integer): PLBox; cdecl; external LIBLEPT;

{*!
 *  boxaGetBoxGeometry()
 *
 *      Input:  boxa
 *              index  (to the index-th box)
 *              &x, &y, &w, &h (<optional return>; each can be null)
 *      Return: 0 if OK, 1 on error
 *}
function boxaGetBoxGeometry ( boxa: PBoxArray; index: Integer; px, py, pw, ph: PInteger): Integer; cdecl; external LIBLEPT;

{*!
 *  boxaReplaceBox()
 *
 *      Input:  boxa
 *              index  (to the index-th box)
 *              box (insert to replace existing one)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) In-place replacement of one box.
 *      (2) The previous box at that location is destroyed.
 *}
function boxaReplaceBox( boxa: PBoxArray; index: Integer; box: PLBox ): Integer; cdecl; external LIBLEPT;

{*!
 *  pixGetData()
 *
 *  Notes:
 *      (1) This gives a new handle for the data.  The data is still
 *          owned by the pix, so do not call FREE() on it.
 *}
function pixGetData( pix: PLPix ): Pointer; cdecl; external LIBLEPT;

function pixGetWpl( pix: PLPix ):Integer; cdecl; external LIBLEPT;

function pixDestroyColormap( pix: PLPix ): Integer; cdecl; external LIBLEPT;

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


{*------------------------------------------------------------------*
 *                       Textblock extraction                       *
 *------------------------------------------------------------------*/
/*!
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
function pixGenTextblockMask( pixs: PLPix; pixvws: PLPix; debug: Integer): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixGetTextlineCenters()
 *
 *      Input:  pixs (1 bpp)
 *              debugflag (1 for debug output)
 *      Return: ptaa (of center values of textlines)
 *
 *  Notes:
 *      (1) This in general does not have a point for each value
 *          of x, because there will be gaps between words.
 *          It doesn't matter because we will fit a quadratic to the
 *          points that we do have.
 *}
function pixGetTextlineCenters( pixs: PLPix; debugflag: longint): PPtaArray;  cdecl; external LIBLEPT;

{*!
 *  boxaaAddBoxa()
 *
 *      Input:  boxaa
 *              boxa     (to be added)
 *              copyflag  (L_INSERT, L_COPY, L_CLONE)
 *      Return: 0 if OK, 1 on error
 *}
function boxaaAddBoxa(baa: PBoxArrayArray; ba: PBoxArray; copyflag: Integer): Integer; cdecl; external LIBLEPT;

{*!
 *  boxaaGetCount()
 *
 *      Input:  boxaa
 *      Return: count (number of boxa), or 0 if no boxa or on error
 *}
function boxaaGetCount(baa: PBoxArrayArray): Integer; cdecl; external LIBLEPT;

{*!
 *  boxaaGetBoxCount()
 *
 *      Input:  boxaa
 *      Return: count (number of boxes), or 0 if no boxes or on error
 *}
function boxaaGetBoxCount(baa: PBoxArrayArray): Integer; cdecl; external LIBLEPT;

{*!
 *  numaGetFValue()
 *
 *      Input:  na
 *              index (into numa)
 *              &val  (<return> float value; 0.0 on error)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Caller may need to check the function return value to
 *          decide if a 0.0 in the returned ival is valid.
 *}
function numaGetFValue( na: PNumArray; index: Longint; pval: PSingle ): Longint; cdecl; external LIBLEPT;

{*!
 *  numaGetIValue()
 *
 *      Input:  na
 *              index (into numa)
 *              &ival  (<return> integer value; 0 on error)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      (1) Caller may need to check the function return value to
 *          decide if a 0 in the returned ival is valid.
 *}
function numaGetIValue( na: PNumArray; index: Longint; pival: PLongint ): Longint; cdecl; external LIBLEPT;

{*!
 *  pixaGetCount()
 *
 *      Input:  pixa
 *      Return: count, or 0 if no pixa
 *}
function pixaGetCount( pixa: PPixArray): Longint; cdecl; external LIBLEPT;





implementation

end.

