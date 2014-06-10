{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *   boxbasic.c                                             PASCAL
 *
 *   Basic 'class' functions for box, boxa and boxaa,
 *   including accessors and serialization.
 *
 *      Box creation, copy, clone, destruction
 *           BOX      *boxCreate()                             X
 *           BOX      *boxCreateValid()                        X
 *           BOX      *boxCopy()
 *           BOX      *boxClone()
 *           void      boxDestroy()                            X
 *
 *      Box accessors
 *           l_int32   boxGetGeometry()                        X
 *           l_int32   boxSetGeometry()                        X
 *           l_int32   boxGetRefcount()
 *           l_int32   boxChangeRefcount()
 *           l_int32   boxIsValid()
 *
 *      Boxa creation, copy, destruction
 *           BOXA     *boxaCreate()
 *           BOXA     *boxaCopy()
 *           void      boxaDestroy()                           X
 *
 *      Boxa array extension
 *           l_int32   boxaAddBox()
 *           l_int32   boxaExtendArray()
 *           l_int32   boxaExtendArrayToSize()
 *
 *      Boxa accessors
 *           l_int32   boxaGetCount()                          X
 *           l_int32   boxaGetValidCount()
 *           BOX      *boxaGetBox()                            X
 *           BOX      *boxaGetValidBox()
 *           l_int32   boxaGetBoxGeometry()                    X
 *
 *      Boxa array modifiers
 *           l_int32   boxaReplaceBox()                        X
 *           l_int32   boxaInsertBox()
 *           l_int32   boxaRemoveBox()
 *           l_int32   boxaInitFull()
 *           l_int32   boxaClear()
 *
 *      Boxaa creation, copy, destruction
 *           BOXAA    *boxaaCreate()                           X
 *           BOXAA    *boxaaCopy()                             X
 *           void      boxaaDestroy()                          X
 *
 *      Boxaa array extension
 *           l_int32   boxaaAddBoxa()                          X
 *           l_int32   boxaaExtendArray()
 *           l_int32   boxaaExtendArrayToSize()
 *
 *      Boxaa accessors
 *           l_int32   boxaaGetCount()                         X
 *           l_int32   boxaaGetBoxCount()                      X
 *           BOXA     *boxaaGetBoxa()
 *           BOX      *boxaaGetBox()
 *
 *      Boxa array modifiers
 *           l_int32   boxaaInitFull()
 *           l_int32   boxaaExtendWithInit()
 *           l_int32   boxaaReplaceBoxa()
 *           l_int32   boxaaInsertBoxa()
 *           l_int32   boxaaRemoveBoxa()
 *           l_int32   boxaaAddBox()
 *
 *      Boxaa serialized I/O
 *           BOXAA    *boxaaRead()
 *           BOXAA    *boxaaReadStream()
 *           l_int32   boxaaWrite()
 *           l_int32   boxaaWriteStream()
 *
 *      Boxa serialized I/O
 *           BOXA     *boxaRead()                              X
 *           BOXA     *boxaReadStream()
 *           l_int32   boxaWrite()                             X
 *           l_int32   boxaWriteStream()
 *
 *      Box print (for debug)
 *           l_int32   boxPrintStreamInfo()
 *
 *   Most functions use only valid boxes, which are boxes that have both
 *   width and height > 0.  However, a few functions, such as
 *   boxaGetMedian() do not assume that all boxes are valid.  For any
 *   function that can use a boxa with invalid boxes, it is convenient
 *   to use these accessors:
 *       boxaGetValidCount()   :  count of valid boxes
 *       boxaGetValidBox()     :  returns NULL for invalid boxes
 *}

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

 {*---------------------------------------------------------------------*
  *             Boxa creation, destruction, copy, extension             *
  *---------------------------------------------------------------------*}

 {*!
  *  boxaDestroy()
  *
  *      Input:  &boxa (<will be set to null before returning>)
  *      Return: void
  *
  *  Note:
  *      - Decrements the ref count and, if 0, destroys the boxa.
  *      - Always nulls the input ptr.
  *}
 procedure boxaDestroy( pboxa: PBoxArray); cdecl; external LIBLEPT;

 {*---------------------------------------------------------------------*
 *                             Boxa accessors                          *
 *---------------------------------------------------------------------*}
{*!
 *  boxaGetCount()
 *
 *      Input:  boxa
 *      Return: count (of all boxes); 0 if no boxes or on error
 *}
function boxaGetCount( boxa: PBoxArray): Integer; cdecl; external LIBLEPT;

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


{*---------------------------------------------------------------------*
 *                        Boxa array modifiers                         *
 *---------------------------------------------------------------------*}

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


{*--------------------------------------------------------------------------*
 *                     Boxaa creation, destruction                          *
 *--------------------------------------------------------------------------*}
{*!
 *  boxaaCreate()
 *
 *      Input:  size of boxa ptr array to be alloc'd (0 for default)
 *      Return: baa, or null on error
 *}
function boxaaCreate(n: Integer):PBoxArrayArray; cdecl; external LIBLEPT;

{*!
 *  boxaaCopy()
 *
 *      Input:  baas (input boxaa to be copied)
 *              copyflag (L_COPY, L_CLONE)
 *      Return: baad (new boxaa, composed of copies or clones of the boxa
 *                    in baas), or null on error
 *
 *  Notes:
 *      (1) L_COPY makes a copy of each boxa in baas.
 *          L_CLONE makes a clone of each boxa in baas.
 *}
function boxaaCopy(baas: PBoxArrayArray; copyflag: Longint): PBoxArrayArray;  cdecl; external LIBLEPT;


{*!
 *  boxaaDestroy()
 *
 *      Input:  @boxaa (<will be set to nil before returning>)
 *      Return: void
 *}
procedure boxaaDestroy(pbaa: PPBoxArrayArray); cdecl; external LIBLEPT;

{*--------------------------------------------------------------------------*
 *                              Add Boxa to Boxaa                           *
 *--------------------------------------------------------------------------*}

 {*!
 *  boxaaAddBoxa()
 *
 *      Input:  boxaa
 *              boxa     (to be added)
 *              copyflag  (L_INSERT, L_COPY, L_CLONE)
 *      Return: 0 if OK, 1 on error
 *}
function boxaaAddBoxa(baa: PBoxArrayArray; ba: PBoxArray; copyflag: Integer): Integer; cdecl; external LIBLEPT;

{*----------------------------------------------------------------------*
 *                           Boxaa accessors                            *
 *----------------------------------------------------------------------*}

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


{*---------------------------------------------------------------------*
 *                         Boxa serialized I/O                         *
 *---------------------------------------------------------------------*}
{*!
 *  boxaRead()
 *
 *      Input:  filename
 *      Return: boxa, or null on error
 *}
function boxaRead( filename: PChar ): PBoxArray;cdecl; external LIBLEPT;

{*!
 *  boxaWrite()
 *
 *      Input:  filename
 *              boxa
 *      Return: 0 if OK, 1 on error
 *}
function boxaWrite(filename: Pchar; boxa: PBoxArray): Integer; cdecl; external LIBLEPT;



