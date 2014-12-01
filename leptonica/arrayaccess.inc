{ This file is an automated translation to pascal of arrayaccess.c            }
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
 *  arrayaccess.c
 *
 *     Access within an array of 32-bit words
 *
 *           l_int32     l_getDataBit()
 *           void        l_setDataBit()
 *           void        l_clearDataBit()
 *           void        l_setDataBitVal()
 *           l_int32     l_getDataDibit()
 *           void        l_setDataDibit()
 *           void        l_clearDataDibit()
 *           l_int32     l_getDataQbit()
 *           void        l_setDataQbit()
 *           void        l_clearDataQbit()
 *           l_int32     l_getDataByte()
 *           void        l_setDataByte()
 *           l_int32     l_getDataTwoBytes()
 *           void        l_setDataTwoBytes()
 *           l_int32     l_getDataFourBytes()
 *           void        l_setDataFourBytes()
 *
 *     Note that these all require 32-bit alignment, and hence an input
 *     ptr to l_uint32.  However, this is not enforced by the compiler.
 *     Instead, we allow the use of a void* ptr, because the line ptrs
 *     are an efficient way to get random access (see pixGetLinePtrs()).
 *     It is then necessary to cast internally within each function
 *     because ptr arithmetic requires knowing the size of the units
 *     being referenced.
 *}

{*----------------------------------------------------------------------*
 *                 Access within an array of 32-bit words               *
 *----------------------------------------------------------------------*}

{*!
 *  l_getDataBit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: val of the nth (1-bit) pixel.
 *}
function l_getDataBit( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataBit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: void
 *
 *  Action: sets the pixel to 1
 *}
procedure l_setDataBit( line: Pointer; n: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_clearDataBit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: void
 *
 *  Action: sets the (1-bit) pixel to 0
 *}
procedure l_clearDataBit( line: Pointer; n: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_setDataBitVal()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 or 1)
 *      Return: void
 *
 *  Notes:
 *      (1) This is an accessor for a 1 bpp pix.
 *      (2) It is actually a little slower than using:
 *            if (val == 0)
 *                l_ClearDataBit(line, n);
 *            else
 *                l_SetDataBit(line, n);
 *}
procedure l_setDataBitVal( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_getDataDibit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: val of the nth (2-bit) pixel.
 *}
function l_getDataDibit( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataDibit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 - 3)
 *      Return: void
 *}
procedure l_setDataDibit( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_clearDataDibit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: void
 *
 *  Action: sets the (2-bit) pixel to 0
 *}
procedure l_clearDataDibit( line: Pointer; n: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_getDataQbit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: val of the nth (4-bit) pixel.
 *}
function l_getDataQbit( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataQbit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 - 0xf)
 *      Return: void
 *}
procedure l_setDataQbit( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_clearDataQbit()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: void
 *
 *  Action: sets the (4-bit) pixel to 0
 *}
procedure l_clearDataQbit( line: Pointer; n: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_getDataByte()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: value of the n-th (byte) pixel
 *}
function l_getDataByte( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataByte()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 - 0xff)
 *      Return: void
 *}
procedure l_setDataByte( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_getDataTwoBytes()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: value of the n-th (2-byte) pixel
 *}
function l_getDataTwoBytes( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataTwoBytes()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 - 0xffff)
 *      Return: void
 *}
procedure l_setDataTwoBytes( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  l_getDataFourBytes()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *      Return: value of the n-th (4-byte) pixel
 *}
function l_getDataFourBytes( line: Pointer; n: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_setDataFourBytes()
 *
 *      Input:  line  (ptr to beginning of data line)
 *              n     (pixel index)
 *              val   (val to be inserted: 0 - 0xffffffff)
 *      Return: void
 *}
procedure l_setDataFourBytes( line: Pointer; n, val: LongInt ) ; cdecl; external LIBLEPT;


