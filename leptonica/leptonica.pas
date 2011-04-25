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
  TLeptPix = Pointer;
  PLeptPix = ^TLeptPix;
  PPLeptPix = ^PLeptPix;

  TPointArray = record
    n: Integer;             //* actual number of pts
    nalloc: Integer;        //* size of allocated arrays
    refcount: Integer;      //* reference count (1 if no clones)
    x, y: Array of Single;  //* arrays of floats
  end;

  PPointArray = ^TPointArray;

  TNumArray = record
    nalloc:         Integer;           //* size of allocated number array
    n:              Integer;           //* number of numbers saved
    refcount:       Integer;           //* reference count (1 if no clones)
    startx:         Single;            //* x value assigned to array[0]
    delx:           Single;            //* change in x value as i --> i + 1
    numarray:       array of Single;   //* number array
  end;

  PNumArray = ^TNumArray;


  function pixRead ( filename: PChar ): PLeptPix; cdecl; external LIBLEPT;
  function pixCreate( w, h, format: Integer ): TLeptPix; cdecl; external LIBLEPT;
  procedure pixDestroy ( pix: PLeptPix ); cdecl; external LIBLEPT;
  procedure numaDestroy ( pna: PNumArray ); cdecl; external LIBLEPT;
  procedure ptaDestroy ( pta: PPointArray ); cdecl; external LIBLEPT;
  function pixGetInputFormat ( Pix: PLeptPix ): Integer; cdecl; external LIBLEPT;
  function pixGetXRes ( Pix: PLeptPix ): Integer; cdecl; external LIBLEPT;
  function pixGetYRes ( Pix: PLeptPix ): Integer; cdecl; external LIBLEPT;
  function pixGetText ( Pix: PLeptPix ): PChar; cdecl; external LIBLEPT;
  function pixWriteStream( fp: Pointer; pix: TLeptPix; imagefileformat: Integer): Integer; cdecl; external LIBLEPT;
  function pixRotate90 (pixs: TLeptPix; rotatedirection: Integer ): TLeptPix; cdecl; external LIBLEPT;
  function pixSobelEdgeFilter ( pixs: PLeptPix; orientflag: Integer ): TleptPix;  cdecl; external LIBLEPT;
  function pixFindBaselines(pixs : TLeptPix; Appta: PPointArray; debug: integer): Pointer; cdecl; external LIBLEPT;
  function ptaWriteStream ( fp: pointer; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;
  function ptaWrite (  filename: PChar; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;
  function pixGetDimensions( pix: TLeptPix; pw, ph, pd: PInteger): Integer; cdecl; external LIBLEPT;

  function pixScale( pix: PLeptPix; pw, ph: Single): TLeptPix; cdecl; external LIBLEPT;
  function pixScaleSmooth( pix: PLeptPix; pw, ph: Single): TLeptPix; cdecl; external LIBLEPT;

 {
  function pixScaleToSize( pix: PLeptPix; wd, hd: Integer): TLeptPix;
  function pixScaleBySampling( pix: PLeptPix; pw, ph: Single): TLeptPix;
  function pixScaleRGBToGrayFast( pix: PLeptPix; pw, ph: Single): TLeptPix;
  function pixReadHeader( filename: PChar; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer;
  function findFileFormat( filename: PChar; pformat: PInteger): Integer;
  function findFileFormatBuffer(buf: PByte ; pformat: PInteger): Integer;
  function pixReadMem(data: PByte; size: PCardinal): PLeptPix;
 }

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
 function pixScaleToSize( pix: PLeptPix; wd, hd: Integer): TLeptPix; cdecl; external LIBLEPT;


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
  function pixScaleBySampling( pix: PLeptPix; pw, ph: Single): TLeptPix; cdecl; external LIBLEPT;

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
  function pixScaleRGBToGrayFast( pix: PLeptPix; pw, ph: Single): TLeptPix; cdecl; external LIBLEPT;



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
  function pixReadMem(data: PByte; size: PCardinal): PLeptPix; cdecl; external LIBLEPT;

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
  function pixReadHeaderMem( data: PByte; size: PCardinal; pformat, pw, ph, pbps, pspp, piscmap: PInteger): Integer; cdecl; external LIBLEPT;

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
  function pixDeskewLocal( pixs: PLeptPix; nslices, redsweep, redsearch: Integer; sweeprange, sweepdelta, minbsdelta: Single): PLeptPix; cdecl; external LIBLEPT;

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
   function pixFindBaselines( pixs: PLeptPix; ppta: PPointArray; debug: Integer = 0): PNumArray; cdecl; external LIBLEPT;

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
   function pixGetRegionsBinary ( pixs: PLeptPix; ppixhm, ppixtm, ppixtb: PPLeptPix; debug: Integer=0): Integer; cdecl; external LIBLEPT;

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
function pixGenHalftoneMask( pixs: PLeptPix; ppixtext: PPLeptPix; phtfound: PInteger; debug: Integer = 0): PLeptPix; cdecl; external LIBLEPT;

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
function pixGenTextlineMask( pixs: PLeptPix; ppixvws: PPLeptPix; ptlfound: PInteger; debug: Integer = 0): PLeptPix; cdecl; external LIBLEPT;

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
function pixThresholdToBinary( pixs: TLeptPix; thresh: Integer): PLeptPix; cdecl; external LIBLEPT;

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
function pixWrite( filename: PChar; pix: TLeptPix; format: Integer): Integer; cdecl; external LIBLEPT;

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
function pixWriteMem(pdata: Pointer{array of byte}; psize: PInteger; pix: PLeptPix; imageformat: Integer): Integer; cdecl; external LIBLEPT;


implementation

end.

