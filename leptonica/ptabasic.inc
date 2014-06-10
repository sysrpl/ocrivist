{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}


{*                                                          TRANSLATED
 *   ptabasic.c
 *
 *      Pta creation, destruction, copy, clone, empty
 *           PTA      *ptaCreate()
 *           PTA      *ptaCreateFromNuma()
 *           void      ptaDestroy()                             X
 *           PTA      *ptaCopy()
 *           PTA      *ptaCopyRange()
 *           PTA      *ptaClone()
 *           l_int32   ptaEmpty()
 *
 *      Pta array extension
 *           l_int32   ptaAddPt()
 *           l_int32   ptaExtendArrays()
 *
 *      Pta Accessors
 *           l_int32   ptaGetRefcount()
 *           l_int32   ptaChangeRefcount()
 *           l_int32   ptaGetCount()
 *           l_int32   ptaGetPt()
 *           l_int32   ptaGetIPt()
 *           l_int32   ptaSetPt()
 *           l_int32   ptaGetArrays()
 *
 *      Pta serialized for I/O
 *           PTA      *ptaRead()
 *           PTA      *ptaReadStream()
 *           l_int32   ptaWrite()                               X
 *           l_int32   ptaWriteStream()                         X
 *
 *      Ptaa creation, destruction
 *           PTAA     *ptaaCreate()
 *           void      ptaaDestroy()
 *
 *      Ptaa array extension
 *           l_int32   ptaaAddPta()
 *           l_int32   ptaaExtendArray()
 *
 *      Ptaa Accessors
 *           l_int32   ptaaGetCount()
 *           l_int32   ptaaGetPta()
 *           l_int32   ptaaGetPt()
 *
 *      Ptaa serialized for I/O
 *           PTAA     *ptaaRead()
 *           PTAA     *ptaaReadStream()
 *           l_int32   ptaaWrite()
 *           l_int32   ptaaWriteStream()
 *}

{*---------------------------------------------------------------------*
 *                Pta creation, destruction, copy, clone               *
 *---------------------------------------------------------------------*}

{*!
 *  ptaDestroy()
 *
 *      Input:  @pta (<to be nulled>)
 *      Return: void
 *
 *  Note:
 *      - Decrements the ref count and, if 0, destroys the pta.
 *      - Always nulls the input ptr.
 *}
procedure ptaDestroy ( pta: PPointArray ); cdecl; external LIBLEPT;

{*---------------------------------------------------------------------*
 *                       Pta serialized for I/O                        *
 *---------------------------------------------------------------------*}

{*!
 *  ptaWriteStream()
 *
 *      Input:  stream
 *              pta
 *              type  (0 for float values; 1 for integer values)
 *      Return: 0 if OK; 1 on error
 *}
function ptaWriteStream ( fp: pointer; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;

{*!
 *  ptaWrite()
 *
 *      Input:  filename
 *              pta
 *              type  (0 for float values; 1 for integer values)
 *      Return: 0 if OK, 1 on error
 *}
function ptaWrite (  filename: PChar; pta: PPointArray; nktype: integer ): integer; cdecl; external LIBLEPT;


