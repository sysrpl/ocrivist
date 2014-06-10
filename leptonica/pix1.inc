{/*====================================================================*
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
 *====================================================================*/
 *
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
 *                                                                   (TRANSLATED)
 *    Pix memory management (allows custom allocator and deallocator)
 *          static void  *pix_malloc()
 *          static void   pix_free()
 *          void          setPixMemoryManager()
 *
 *    Pix creation
 *          PIX          *pixCreate()                                     X
 *          PIX          *pixCreateNoInit()
 *          PIX          *pixCreateTemplate()
 *          PIX          *pixCreateTemplateNoInit()
 *          PIX          *pixCreateHeader()
 *          PIX          *pixClone()                                      X
 *
 *    Pix destruction
 *          void          pixDestroy()                                    X
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
 *          l_int32       pixGetWidth()                                   X
 *          l_int32       pixSetWidth()                                   X
 *          l_int32       pixGetHeight()                                  X
 *          l_int32       pixSetHeight()                                  X
 *          l_int32       pixGetDepth()                                   X
 *          l_int32       pixSetDepth()                                   X
 *          l_int32       pixGetDimensions()                              X
 *          l_int32       pixSetDimensions()                              X
 *          l_int32       pixCopyDimensions()
 *          l_int32       pixGetWpl()
 *          l_int32       pixSetWpl()
 *          l_int32       pixGetRefcount()
 *          l_int32       pixChangeRefcount()
 *          l_uint32      pixGetXRes()                                    X
 *          l_int32       pixSetXRes()
 *          l_uint32      pixGetYRes()                                    X
 *          l_int32       pixSetYRes()
 *          l_int32       pixGetResolution()                              X
 *          l_int32       pixSetResolution()                              X
 *          l_int32       pixCopyResolution()
 *          l_int32       pixScaleResolution()
 *          l_int32       pixGetInputFormat()                             X
 *          l_int32       pixSetInputFormat()
 *          l_int32       pixCopyInputFormat()
 *          char         *pixGetText()                                    X
 *          l_int32       pixSetText()                                    X
 *          l_int32       pixAddText()                                    X
 *          l_int32       pixCopyText()
 *          PIXCMAP      *pixGetColormap()                                X
 *          l_int32       pixSetColormap()
 *          l_int32       pixDestroyColormap()                            X
 *          l_uint32     *pixGetData()                                    X
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

 {
 *--------------------------------------------------------------------*
 *                              Pix Creation                          *
 *--------------------------------------------------------------------*}
{*!
 *  pixCreate()
 *
 *      Input:  width, height, depth
 *      Return: pixd (with data allocated and initialized to 0),
 *                    or null on error
 *}
 function pixCreate( w, h, format: Integer ): TLPix; cdecl; external LIBLEPT;

 {/*!
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
 function pixClone ( pixs: PLPix ): PLPix; cdecl; external LIBLEPT;

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
 procedure pixDestroy ( pix: PLPix ); cdecl; external LIBLEPT;

{*--------------------------------------------------------------------*
 *                                Accessors                           *
 *--------------------------------------------------------------------*}

 function pixGetWidth( pix: PLPix ): Longint; cdecl; external LIBLEPT;

 function pixSetWidth( pix: PLPix; width: Longint ): Longint; cdecl; external LIBLEPT;

 function pixGetHeight( pix: PLPix ): Longint; cdecl; external LIBLEPT;

 function pixSetHeight( pix: PLPix; height: Longint ): Longint; cdecl; external LIBLEPT;

 function pixGetDepth( pix: PLPix): Longint; cdecl; external LIBLEPT;

 function pixSetDepth( pix: PLPix; depth: Longint): Longint; cdecl; external LIBLEPT;

{*!
 *  pixGetDimensions()
 *
 *      Input:  pix
 *              @w, @h, @d (<optional return>; each can be nil)
 *      Return: 0 if OK, 1 on error
 *}
function pixGetDimensions( pix: PLPix; pw, ph, pd: PLongint): LOngint; cdecl; external LIBLEPT;

{*!
 *  pixSetDimensions()
 *
 *      Input:  pix
 *              w, h, d (use 0 to skip the setting for any of these)
 *      Return: 0 if OK, 1 on error
 *}
 function pixSetDimensions( pix: PLPix; w, h, d: Longint): Longint; cdecl; external LIBLEPT;

 function pixGetWpl( pix: PLPix ):Integer; cdecl; external LIBLEPT;

 function pixGetXRes ( Pix: PLPix ): Integer; cdecl; external LIBLEPT;
 function pixGetYRes ( Pix: PLPix ): Integer; cdecl; external LIBLEPT;
 function pixSetResolution ( pix: PLPix; xres, yres: Integer ): Integer; cdecl; external LIBLEPT;

{*!
 *  pixGetResolution()
 *
 *      Input:  pix
 *              @xres, @yres (<optional return>; each can be nil)
 *      Return: 0 if OK, 1 on error
 *}
 function pixGetResolution ( pix: PLPix; pxres, pyres: PLongint ): Longint; cdecl; external LIBLEPT;

 function pixGetInputFormat ( pix: PLPix ): Longint; cdecl; external LIBLEPT;


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
 function pixSetText( pix: PLPix; textstring: PChar ): Longint; cdecl; external LIBLEPT;

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
 function pixAddText( pix: PLPix; textstring: PChar ): Longint; cdecl; external LIBLEPT;

 function pixGetColormap( pix: PLPix ): PPixCmap; cdecl; external LIBLEPT;

 {*!
 *  pixDestroyColormap()
 *
 *      Input:  pix
 *      Return: 0 if OK, 1 on error
 *}
 function pixDestroyColormap( pix: PLPix ): Integer; cdecl; external LIBLEPT;

 {*!
 *  pixGetData()
 *
 *  Notes:
 *      (1) This gives a new handle for the data.  The data is still
 *          owned by the pix, so do not call FREE() on it.
 *}
function pixGetData( pix: PLPix ): Pointer; cdecl; external LIBLEPT;







