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

      REMOVE_CMAP_TO_BINARY     = 0;
      REMOVE_CMAP_TO_GRAYSCALE  = 1;
      REMOVE_CMAP_TO_FULL_COLOR = 2;
      REMOVE_CMAP_BASED_ON_SRC  = 3;



// === pix.h ===

{*-------------------------------------------------------------------------*
 *                             Colors for 32 bpp                           *
 *-------------------------------------------------------------------------*/
/*  Notes:
 *      (1) These are the byte indices for colors in 32 bpp images.
 *          They are used through the GET/SET_DATA_BYTE accessors.
 *          The 4th byte, typically known as the "alpha channel" and used
 *          for blending, is not explicitly used in leptonica.
 *      (2) If you redefine these values, functions that have the shifts
 *          hardcoded (instead of using the constants below) will break.
 *          These functions are labelled with "***" next to their names
 *          at the top of the files in which they are defined.
 *          Advice: Do not change these values!
 *      (3) The shifts to extract the red, green and blue components
 *          from a 32 bit pixel are defined in terms of these colors.
 *}
const
        COLOR_RED       = 0;
        OLOR_GREEN      = 1;
        COLOR_BLUE      = 2;
        L_ALPHA_CHANNEL = 3;

{*...........................................................................*
 *                            Operation bit flags
 *...........................................................................*}

        PIX_CLR                   = $0;
        PIX_SET                   = $1e;
        PIX_SRC                   = $18;
        PIX_DST                   = $14;



type

  L_BMF = Pointer;
  PPSingle = ^PSingle;
  PPLongWord = ^PLongWord;

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

  PixColormap = record
      RGBAarray: Pointer;              // colormap table (array of RGBA_QUAD)
      depth: LongInt;                  // of pix (1, 2, 4 or 8 bpp)
      nalloc: LongInt;                 // number of color entries allocated
      n: LongInt;                      // number of color entries used
  end;
  PPixCmap = ^PixColormap;
  PPPixCmap = ^PPixCmap;

  RGBA_Quad = record
    blue: Byte;
    green: Byte;
    red: Byte;
    reserved: Byte;
  end;

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
  PPNumArray = ^PNumArray;
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

  // String array: an array of C strings
  PSArray = ^TSArray;
  TSarray = record
     nalloc:     Integer;           // size of allocated ptr array
     n:          Integer;           // number of strings allocated
     refcount:   Integer;           // reference count (1 if no clones)
     strarray:   array of PChar;    // string array
  end;


  {*-------------------------------------------------------------------------*
   *                    Flags for adding text to a pix                       *
   *-------------------------------------------------------------------------*}
 const
      L_ADD_ABOVE = 1;           // Add text above the image
      L_ADD_AT_TOP = 2;          // Add text over the top of the image
      L_ADD_AT_BOTTOM = 3;       // Add text over the bottom of the image
      L_ADD_BELOW = 4;           // Add text below the image



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

    {*-------------------------------------------------------------------------*
     *                        Graphics pixel setting                           *
     *-------------------------------------------------------------------------*}
const
        L_SET_PIXELS   = 1;            // set all bits in each pixel to 1
        L_CLEAR_PIXELS = 2;            // set all bits in each pixel to 0
        L_FLIP_PIXELS  = 3;            // flip all bits in each pixel

{$I adaptmap.inc}
{$I affine.inc}
{$I affinecompose.inc}
{$I arithlow.inc}
{$I arrayaccess.inc}
{$I baseline.inc}
{$I bilinear.inc}
{$I binarize.inc}
{$I binexpand.inc}
{$I binreduce.inc}
{$I bmf.inc}
{$I boxbasic.inc}
{$I boxfunc1.inc}
{$I classapp.inc}
{$I colormap.inc}
{$I colorseg.inc}
{$I compare.inc}
{$I conncomp.inc}
//{$I dewarp.inc}

{$I edge.inc}
{$I enhance.inc}
{$I graphics.inc}
{$I graymorph.inc}
{$I grayquant.inc}
{$I morph.inc}
{$I morphseq.inc}
{$I numabasic.inc}
{$I pageseg.inc}
{$I pix1.inc}

{$I pix2.inc}
{$I pix3.inc}
{$I pix4.inc}
{$I pix5.inc}
{$I pdfio.inc}
{$I pixabasic.inc}
{$I pixafunc2.inc}
{$I pixarith.inc}
{$I pixconv.inc}
{$I ptabasic.inc}
//{$I ptafunc.inc}
{$I readfile.inc}

{$I rop.inc}
{$I rotate.inc}
{$I rotateorth.inc}
{$I rotateshear.inc}
{$I scale.inc}
{$I seedfill.inc}
{$I skew.inc}
//{$I textops.inc}
{$I writefile.inc}



implementation

end.

