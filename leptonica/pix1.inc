{ This file is an automated translation to pascal of pix1.c                   }
{ performed using the Leptonica Library Converter, written by Malcolm Poole   }

{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 -
 -  Redistribution and use in source and binary forms, with or without
 -  modification, are permitted provided that the following conditions
 -  are met:
 -  1. Redistributions of source code must retain the above copyright
 -     notice, this list of conditions and the following disclaimer.
 -  2. Redistributions in binary form must reproduce the above
 -     copyright notice, this list of conditions and the following
 -     disclaimer in the documentation and/or other materials
 -     provided with the distribution.
 -
 -  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 -  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 -  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 -  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL ANY
 -  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 -  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 -  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 -  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 -  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 -  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 -  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *====================================================================*}

{*
 *  pix1.c
 *
 *    The pixN.c {N = 1,2,3,4,5} files are sorted by the type of operation.
 *    The primary functions in these files are:
 *
 *        pix1.c: constructors, destructors and field accessors
 *        pix2.c: pixel poking of image, pad and border pixels
 *        pix3.c: masking and logical ops, counting, mirrored tiling
 *        pix4.c: histograms, statistics, fg/bg estimation
 *        pix5.c: property measurements, rectangle extraction
 *
 *
 *    This file has the basic constructors, destructors and field accessors
 *
 *    Pix memory management (allows custom allocator and deallocator)
 *          static void  *pix_malloc()
 *          static void   pix_free()
 *          void          setPixMemoryManager()
 *
 *    Pix creation
 *          PIX          *pixCreate()
 *          PIX          *pixCreateNoInit()
 *          PIX          *pixCreateTemplate()
 *          PIX          *pixCreateTemplateNoInit()
 *          PIX          *pixCreateHeader()
 *          PIX          *pixClone()
 *
 *    Pix destruction
 *          void          pixDestroy()
 *          static void   pixFree()
 *
 *    Pix copy
 *          PIX          *pixCopy()
 *          l_int32       pixResizeImageData()
 *          l_int32       pixCopyColormap()
 *          l_int32       pixSizesEqual()
 *          l_int32       pixTransferAllData()
 *          l_int32       pixSwapAndDestroy()
 *
 *    Pix accessors
 *          l_int32       pixGetWidth()
 *          l_int32       pixSetWidth()
 *          l_int32       pixGetHeight()
 *          l_int32       pixSetHeight()
 *          l_int32       pixGetDepth()
 *          l_int32       pixSetDepth()
 *          l_int32       pixGetDimensions()
 *          l_int32       pixSetDimensions()
 *          l_int32       pixCopyDimensions()
 *          l_int32       pixGetWpl()
 *          l_int32       pixSetWpl()
 *          l_int32       pixGetRefcount()
 *          l_int32       pixChangeRefcount()
 *          l_uint32      pixGetXRes()
 *          l_int32       pixSetXRes()
 *          l_uint32      pixGetYRes()
 *          l_int32       pixSetYRes()
 *          l_int32       pixGetResolution()
 *          l_int32       pixSetResolution()
 *          l_int32       pixCopyResolution()
 *          l_int32       pixScaleResolution()
 *          l_int32       pixGetInputFormat()
 *          l_int32       pixSetInputFormat()
 *          l_int32       pixCopyInputFormat()
 *          char         *pixGetText()
 *          l_int32       pixSetText()
 *          l_int32       pixAddText()
 *          l_int32       pixCopyText()
 *          PIXCMAP      *pixGetColormap()
 *          l_int32       pixSetColormap()
 *          l_int32       pixDestroyColormap()
 *          l_uint32     *pixGetData()
 *          l_int32       pixSetData()
 *          l_uint32     *pixExtractData()
 *          l_int32       pixFreeData()
 *
 *    Pix line ptrs
 *          void        **pixGetLinePtrs()
 *
 *    Pix debug
 *          l_int32       pixPrintStreamInfo()
 *
 *
 *  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *      Important notes on direct management of pix image data
 *  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *
 *  Custom allocator and deallocator
 *  --------------------------------
 *
 *  At the lowest level, you can specify the function that does the
 *  allocation and deallocation of the data field in the pix.
 *  By default, this is malloc and free.  However, by calling
 *  setPixMemoryManager(), custom functions can be substituted.
 *  When using this, keep two things in mind:
 *
 *   (1) Call setPixMemoryManager() before any pix have been allocated
 *   (2) Destroy all pix as usual, in order to prevent leaks.
 *
 *  In pixalloc.c, we provide an example custom allocator and deallocator.
 *  To use it, you must call pmsCreate() before any pix have been allocated
 *  and pmsDestroy() at the end after all pix have been destroyed.
 *
 *
 *  Direct manipulation of the pix data field
 *  -----------------------------------------
 *
 *  Memory management of the (image) data field in the pix is
 *  handled differently from that in the colormap or text fields.
 *  For colormap and text, the functions pixSetColormap() and
 *  pixSetText() remove the existing heap data and insert the
 *  new data.  For the image data, pixSetData() just reassigns the
 *  data field; any existing data will be lost if there isn't
 *  another handle for it.
 *
 *  Why is pixSetData() limited in this way?  Because the image
 *  data can be very large, we need flexible ways to handle it,
 *  particularly when you want to re-use the data in a different
 *  context without making a copy.  Here are some different
 *  things you might want to do:
 *
 *  (1) Use pixCopy(pixd, pixs) where pixd is not the same size
 *      as pixs.  This will remove the data in pixd, allocate a
 *      new data field in pixd, and copy the data from pixs, leaving
 *      pixs unchanged.
 *
 *  (2) Use pixTransferAllData(pixd, &pixs, ...) to transfer the
 *      data from pixs to pixd without making a copy of it.  If
 *      pixs is not cloned, this will do the transfer and destroy pixs.
 *      But if the refcount of pixs is greater than 1, it just copies
 *      the data and decrements the ref count.
 *
 *  (3) Use pixSwapAndDestroy(pixd, &pixs) to replace pixs by an
 *      existing pixd.  This is similar to pixTransferAllData(), but
 *      simpler, in that it never makes any copies and if pixs is
 *      cloned, the other references are not changed by this operation.
 *
 *  (4) Use pixExtractData() to extract the image data from the pix
 *      without copying if possible.  This could be used, for example,
 *      to convert from a pix to some other data structure with minimal
 *      heap allocation.  After the data is extracated, the pixels can
 *      be munged and used in another context.  However, the danger
 *      here is that the pix might have a refcount > 1, in which case
 *      a copy of the data must be made and the input pix left unchanged.
 *      If there are no clones, the image data can be extracted without
 *      a copy, and the data ptr in the pix must be nulled before
 *      destroying it because the pix will no longer 'own' the data.
 *
 *  We have provided accessors and functions here that should be
 *  sufficient so that you can do anything you want without
 *  explicitly referencing any of the pix member fields.
 *
 *  However, to avoid memory smashes and leaks when doing special operations
 *  on the pix data field, look carefully at the behavior of the image
 *  data accessors and keep in mind that when you invoke pixDestroy(),
 *  the pix considers itself the owner of all its heap data.
 *}

{*-------------------------------------------------------------------------*
 *                        Pix Memory Management                            *
 *                                                                         *
 *  These functions give you the freedom to specify at compile or run      *
 *  time the allocator and deallocator to be used for pix.  It has no      *
 *  effect on memory management for other data structs, which are          *
 *  controlled by the #defines in environ.h.  Likewise, the #defines       *
 *  in environ.h have no effect on the pix memory management.              *
 *  The default functions are malloc and free.  Use setPixMemoryManager()  *
 *  to specify other functions to use.                                     *
 *-------------------------------------------------------------------------*}

{*!
 *  setPixMemoryManager()
 *
 *      Input: allocator (<optional>; use null to skip)
 *             deallocator (<optional>; use null to skip)
 *      Return: void
 *
 *  Notes:
 *      (1) Use this to change the alloc and/or dealloc functions;
 *          e.g., setPixMemoryManager(my_malloc, my_free).
 *}
procedure setPixMemoryManager( allocator, deallocator: Pointer ) ; cdecl; external LIBLEPT;



{*--------------------------------------------------------------------*
 *                              Pix Creation                          *
 *--------------------------------------------------------------------*}

{*!
 *  pixCreate()
 *
 *      Input:  width, height, depth
 *      Return: pixd (with data allocated and initialized to 0),
 *                    or null on error
 *}
function pixCreate( width, height, depth: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCreateNoInit()
 *
 *      Input:  width, height, depth
 *      Return: pixd (with data allocated but not initialized),
 *                    or null on error
 *
 *  Notes:
 *      (1) Must set pad bits to avoid reading unitialized data, because
 *          some optimized routines (e.g., pixConnComp()) read from pad bits.
 *}
function pixCreateNoInit( width, height, depth: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCreateTemplate()
 *
 *      Input:  pixs
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Makes a Pix of the same size as the input Pix, with the
 *          data array allocated and initialized to 0.
 *      (2) Copies the other fields, including colormap if it exists.
 *}
function pixCreateTemplate( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCreateTemplateNoInit()
 *
 *      Input:  pixs
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Makes a Pix of the same size as the input Pix, with
 *          the data array allocated but not initialized to 0.
 *      (2) Copies the other fields, including colormap if it exists.
 *}
function pixCreateTemplateNoInit( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCreateHeader()
 *
 *      Input:  width, height, depth
 *      Return: pixd (with no data allocated), or null on error
 *}
function pixCreateHeader( width, height, depth: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixClone()
 *
 *      Input:  pix
 *      Return: same pix (ptr), or null on error
 *
 *  Notes:
 *      (1) A "clone" is simply a handle (ptr) to an existing pix.
 *          It is implemented because (a) images can be large and
 *          hence expensive to copy, and (b) extra handles to a data
 *          structure need to be made with a simple policy to avoid
 *          both double frees and memory leaks.  Pix are reference
 *          counted.  The side effect of pixClone() is an increase
 *          by 1 in the ref count.
 *      (2) The protocol to be used is:
 *          (a) Whenever you want a new handle to an existing image,
 *              call pixClone(), which just bumps a ref count.
 *          (b) Always call pixDestroy() on all handles.  This
 *              decrements the ref count, nulls the handle, and
 *              only destroys the pix when pixDestroy() has been
 *              called on all handles.
 *}
function pixClone( pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *                           Pix Destruction                          *
 *--------------------------------------------------------------------*}

{*!
 *  pixDestroy()
 *
 *      Input:  &pix <will be nulled>
 *      Return: void
 *
 *  Notes:
 *      (1) Decrements the ref count and, if 0, destroys the pix.
 *      (2) Always nulls the input ptr.
 *}
procedure pixDestroy( ppix: PPLPix ) ; cdecl; external LIBLEPT;


{*!
 *  pixFree()
 *
 *      Input:  pix
 *      Return: void
 *
 *  Notes:
 *      (1) Decrements the ref count and, if 0, destroys the pix.
 *}

{*-------------------------------------------------------------------------*
 *                                 Pix Copy                                *
 *-------------------------------------------------------------------------*}

{*!
 *  pixCopy()
 *
 *      Input:  pixd (<optional>; can be null, or equal to pixs,
 *                    or different from pixs)
 *              pixs
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) There are three cases:
 *            (a) pixd == null  (makes a new pix; refcount = 1)
 *            (b) pixd == pixs  (no-op)
 *            (c) pixd != pixs  (data copy; no change in refcount)
 *          If the refcount of pixd > 1, case (c) will side-effect
 *          these handles.
 *      (2) The general pattern of use is:
 *             pixd = pixCopy(pixd, pixs);
 *          This will work for all three cases.
 *          For clarity when the case is known, you can use:
 *            (a) pixd = pixCopy(NULL, pixs);
 *            (c) pixCopy(pixd, pixs);
 *      (3) For case (c), we check if pixs and pixd are the same
 *          size (w,h,d).  If so, the data is copied directly.
 *          Otherwise, the data is reallocated to the correct size
 *          and the copy proceeds.  The refcount of pixd is unchanged.
 *      (4) This operation, like all others that may involve a pre-existing
 *          pixd, will side-effect any existing clones of pixd.
 *}
function pixCopy( pixd, pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixResizeImageData()
 *
 *      Input:  pixd (gets new uninitialized buffer for image data)
 *              pixs (determines the size of the buffer; not changed)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This removes any existing image data from pixd and
 *          allocates an uninitialized buffer that will hold the
 *          amount of image data that is in pixs.
 *}
function pixResizeImageData( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCopyColormap()
 *
 *      Input:  src and dest Pix
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This always destroys any colormap in pixd (except if
 *          the operation is a no-op.
 *}
function pixCopyColormap( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSizesEqual()
 *
 *      Input:  two pix
 *      Return: 1 if the two pix have same {h, w, d}; 0 otherwise.
 *}
function pixSizesEqual( pix1, pix2: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixTransferAllData()
 *
 *      Input:  pixd (must be different from pixs)
 *              &pixs (will be nulled if refcount goes to 0)
 *              copytext (1 to copy the text field; 0 to skip)
 *              copyformat (1 to copy the informat field; 0 to skip)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) This does a complete data transfer from pixs to pixd,
 *          followed by the destruction of pixs (refcount permitting).
 *      (2) If the refcount of pixs is 1, pixs is destroyed.  Otherwise,
 *          the data in pixs is copied (rather than transferred) to pixd.
 *      (3) This operation, like all others with a pre-existing pixd,
 *          will side-effect any existing clones of pixd.  The pixd
 *          refcount does not change.
 *      (4) When might you use this?  Suppose you have an in-place Pix
 *          function (returning void) with the typical signature:
 *              void function-inplace(PIX *pix, ...)
 *          where "..." are non-pointer input parameters, and suppose
 *          further that you sometimes want to return an arbitrary Pix
 *          in place of the input Pix.  There are two ways you can do this:
 *          (a) The straightforward way is to change the function
 *              signature to take the address of the Pix ptr:
 *                  void function-inplace(PIX **ppix, ...) {
 *                      PIX *pixt = function-makenew(*ppix);
 *                      pixDestroy(ppix);
 *                      *ppix = pixt;
 *                      return;
 *                  }
 *              Here, the input and returned pix are different, as viewed
 *              by the calling function, and the inplace function is
 *              expected to destroy the input pix to avoid a memory leak.
 *          (b) Keep the signature the same and use pixTransferAllData()
 *              to return the new Pix in the input Pix struct:
 *                  void function-inplace(PIX *pix, ...) {
 *                      PIX *pixt = function-makenew(pix);
 *                      pixTransferAllData(pix, &pixt, 0, 0);
 *                               // pixDestroy() is called on pixt
 *                      return;
 *                  }
 *              Here, the input and returned pix are the same, as viewed
 *              by the calling function, and the inplace function must
 *              never destroy the input pix, because the calling function
 *              maintains an unchanged handle to it.
 *}
function pixTransferAllData( pixd: PLPix; ppixs: PPLPix; copytext, copyformat: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSwapAndDestroy()
 *
 *      Input:  &pixd (<optional, return> input pixd can be null,
 *                     and it must be different from pixs)
 *              &pixs (will be nulled after the swap)
 *      Return: 0 if OK, 1 on error
 *
 *  Notes:
 *      (1) Simple operation to change the handle name safely.
 *          After this operation, the original image in pixd has
 *          been destroyed, pixd points to what was pixs, and
 *          the input pixs ptr has been nulled.
 *      (2) This works safely whether or not pixs and pixd are cloned.
 *          If pixs is cloned, the other handles still point to
 *          the original image, with the ref count reduced by 1.
 *      (3) Usage example:
 *            Pix *pix1 = pixRead("...");
 *            Pix *pix2 = function(pix1, ...);
 *            pixSwapAndDestroy(&pix1, &pix2);
 *            pixDestroy(&pix1);  // holds what was in pix2
 *          Example with clones ([] shows ref count of image generated
 *                               by the function):
 *            Pix *pixs = pixRead("...");
 *            Pix *pix1 = pixClone(pixs);
 *            Pix *pix2 = function(pix1, ...);   [1]
 *            Pix *pix3 = pixClone(pix2);   [1] --> [2]
 *            pixSwapAndDestroy(&pix1, &pix2);
 *            pixDestroy(&pixs);  // still holds read image
 *            pixDestroy(&pix1);  // holds what was in pix2  [2] --> [1]
 *            pixDestroy(&pix3);  // holds what was in pix2  [1] --> [0]
 *}
function pixSwapAndDestroy( ppixd, ppixs: PPLPix ) : LongInt; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *                                Accessors                           *
 *--------------------------------------------------------------------*}
function pixGetWidth( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetWidth( pix: PLPix; width: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixGetHeight( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetHeight( pix: PLPix; height: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixGetDepth( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetDepth( pix: PLPix; depth: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetDimensions()
 *
 *      Input:  pix
 *              &w, &h, &d (<optional return>; each can be null)
 *      Return: 0 if OK, 1 on error
 *}
function pixGetDimensions( pix: PLPix; pw, ph, pd: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetDimensions()
 *
 *      Input:  pix
 *              w, h, d (use 0 to skip the setting for any of these)
 *      Return: 0 if OK, 1 on error
 *}
function pixSetDimensions( pix: PLPix; w, h, d: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixCopyDimensions()
 *
 *      Input:  pixd
 *              pixd
 *      Return: 0 if OK, 1 on error
 *}
function pixCopyDimensions( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixGetWpl( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetWpl( pix: PLPix; wpl: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixGetRefcount( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixChangeRefcount( pix: PLPix; delta: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixGetXRes( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetXRes( pix: PLPix; res: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixGetYRes( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetYRes( pix: PLPix; res: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetResolution()
 *
 *      Input:  pix
 *              &xres, &yres (<optional return>; each can be null)
 *      Return: 0 if OK, 1 on error
 *}
function pixGetResolution( pix: PLPix; pxres, pyres: PLongint ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixSetResolution()
 *
 *      Input:  pix
 *              xres, yres (use 0 to skip the setting for either of these)
 *      Return: 0 if OK, 1 on error
 *}
function pixSetResolution( pix: PLPix; xres, yres: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixCopyResolution( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixScaleResolution( pix: PLPix; xscale, yscale: Single ) : LongInt; cdecl; external LIBLEPT;

function pixGetInputFormat( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixSetInputFormat( pix: PLPix; informat: LongInt ) : LongInt; cdecl; external LIBLEPT;

function pixCopyInputFormat( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;


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
function pixGetText( pix: PLPix ) : PChar; cdecl; external LIBLEPT;


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
function pixSetText( pix: PLPix; textstring: PChar ) : LongInt; cdecl; external LIBLEPT;


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
function pixAddText( pix: PLPix; textstring: PChar ) : LongInt; cdecl; external LIBLEPT;

function pixCopyText( pixd, pixs: PLPix ) : LongInt; cdecl; external LIBLEPT;

function pixGetColormap( pix: PLPix ) : PPixCmap; cdecl; external LIBLEPT;


{*!
 *  pixSetColormap()
 *
 *      Input:  pix
 *              colormap (to be assigned)
 *      Return: 0 if OK, 1 on error.
 *
 *  Notes:
 *      (1) Unlike with the pix data field, pixSetColormap() destroys
 *          any existing colormap before assigning the new one.
 *          Because colormaps are not ref counted, it is important that
 *          the new colormap does not belong to any other pix.
 *}
function pixSetColormap( pix: PLPix; colormap: PPixCmap ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixDestroyColormap()
 *
 *      Input:  pix
 *      Return: 0 if OK, 1 on error
 *}
function pixDestroyColormap( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixGetData()
 *
 *  Notes:
 *      (1) This gives a new handle for the data.  The data is still
 *          owned by the pix, so do not call FREE() on it.
 *}
function pixGetData( pix: PLPix ) : PLongWord; cdecl; external LIBLEPT;


{*!
 *  pixSetData()
 *
 *  Notes:
 *      (1) This does not free any existing data.  To free existing
 *          data, use pixFreeData() before pixSetData().
 *}
function pixSetData( pix: PLPix; data: PLongWord ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  pixExtractData()
 *
 *  Notes:
 *      (1) This extracts the pix image data for use in another context.
 *          The caller still needs to use pixDestroy() on the input pix.
 *      (2) If refcount == 1, the data is extracted and the
 *          pix->data ptr is set to NULL.
 *      (3) If refcount > 1, this simply returns a copy of the data,
 *          using the pix allocator, and leaving the input pix unchanged.
 *}
function pixExtractData( pixs: PLPix ) : PLongWord; cdecl; external LIBLEPT;


{*!
 *  pixFreeData()
 *
 *  Notes:
 *      (1) This frees the data and sets the pix data ptr to null.
 *          It should be used before pixSetData() in the situation where
 *          you want to free any existing data before doing
 *          a subsequent assignment with pixSetData().
 *}
function pixFreeData( pix: PLPix ) : LongInt; cdecl; external LIBLEPT;


{*--------------------------------------------------------------------*
 *                          Pix line ptrs                             *
 *--------------------------------------------------------------------*}

{*!
 *  pixGetLinePtrs()
 *
 *      Input:  pix
 *              &size (<optional return> array size, which is the pix height)
 *      Return: array of line ptrs, or null on error
 *
 *  Notes:
 *      (1) This is intended to be used for fast random pixel access.
 *          For example, for an 8 bpp image,
 *              val = GET_DATA_BYTE(lines8[i], j);
 *          is equivalent to, but much faster than,
 *              pixGetPixel(pix, j, i, &val);
 *      (2) How much faster?  For 1 bpp, it's from 6 to 10x faster.
 *          For 8 bpp, it's an amazing 30x faster.  So if you are
 *          doing random access over a substantial part of the image,
 *          use this line ptr array.
 *      (3) When random access is used in conjunction with a stack,
 *          queue or heap, the overall computation time depends on
 *          the operations performed on each struct that is popped
 *          or pushed, and whether we are using a priority queue (O(logn))
 *          or a queue or stack (O(1)).  For example, for maze search,
 *          the overall ratio of time for line ptrs vs. pixGet/Set* is
 *             Maze type     Type                   Time ratio
 *               binary      queue                     0.4
 *               gray        heap (priority queue)     0.6
 *      (4) Because this returns a void** and the accessors take void*,
 *          the compiler cannot check the pointer types.  It is
 *          strongly recommended that you adopt a naming scheme for
 *          the returned ptr arrays that indicates the pixel depth.
 *          (This follows the original intent of Simonyi's "Hungarian"
 *          application notation, where naming is used proactively
 *          to make errors visibly obvious.)  By doing this, you can
 *          tell by inspection if the correct accessor is used.
 *          For example, for an 8 bpp pixg:
 *              void **lineg8 = pixGetLinePtrs(pixg, NULL);
 *              val = GET_DATA_BYTE(lineg8[i], j);  // fast access; BYTE, 8
 *              ...
 *              FREE(lineg8);  // don't forget this
 *      (5) These are convenient for accessing bytes sequentially in an
 *          8 bpp grayscale image.  People who write image processing code
 *          on 8 bpp images are accustomed to grabbing pixels directly out
 *          of the raster array.  Note that for little endians, you first
 *          need to reverse the byte order in each 32-bit word.
 *          Here's a typical usage pattern:
 *              pixEndianByteSwap(pix);   // always safe; no-op on big-endians
 *              l_uint8 **lineptrs = (l_uint8 **)pixGetLinePtrs(pix, NULL);
 *              pixGetDimensions(pix, &w, &h, NULL);
 *              for (i = 0; i < h; i++) {
 *                  l_uint8 *line = lineptrs[i];
 *                  for (j = 0; j < w; j++) {
 *                      val = line[j];
 *                      ...
 *                  }
 *              }
 *              pixEndianByteSwap(pix);  // restore big-endian order
 *              FREE(lineptrs);
 *          This can be done even more simply as follows:
 *              l_uint8 **lineptrs = pixSetupByteProcessing(pix, &w, &h);
 *              for (i = 0; i < h; i++) {
 *                  l_uint8 *line = lineptrs[i];
 *                  for (j = 0; j < w; j++) {
 *                      val = line[j];
 *                      ...
 *                  }
 *              }
 *              pixCleanupByteProcessing(pix, lineptrs);
 *}

{*--------------------------------------------------------------------*
 *                    Print output for debugging                      *
 *--------------------------------------------------------------------*}

{*!
 *  pixPrintStreamInfo()
 *
 *      Input:  fp (file stream)
 *              pix
 *              text (<optional> identifying string; can be null)
 *      Return: 0 if OK, 1 on error
 *}
function pixPrintStreamInfo( fp: Pointer; pix: PLPix; text: PChar ) : LongInt; cdecl; external LIBLEPT;


