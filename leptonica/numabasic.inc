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
 *   numabasic.c                                            TRANSLATED
 *
 *      Numa creation, destruction, copy, clone, etc.
 *          NUMA        *numaCreate()
 *          NUMA        *numaCreateFromIArray()
 *          NUMA        *numaCreateFromFArray()
 *          void        *numaDestroy()                           X
 *          NUMA        *numaCopy()
 *          NUMA        *numaClone()
 *          l_int32      numaEmpty()
 *
 *      Add/remove number (float or integer)
 *          l_int32      numaAddNumber()
 *          l_int32      numaExtendArray()
 *          l_int32      numaInsertNumber()
 *          l_int32      numaRemoveNumber()
 *          l_int32      numaReplaceNumber()
 *
 *      Numa accessors
 *          l_int32      numaGetCount()
 *          l_int32      numaSetCount()
 *          l_int32      numaGetIValue()                         X
 *          l_int32      numaGetFValue()                         X
 *          l_int32      numaSetValue()
 *          l_int32      numaShiftValue()
 *          l_int32     *numaGetIArray()
 *          l_float32   *numaGetFArray()
 *          l_int32      numaGetRefcount()
 *          l_int32      numaChangeRefcount()
 *          l_int32      numaGetXParameters()
 *          l_int32      numaSetXParameters()
 *          l_int32      numaCopyXParameters()
 *
 *      Serialize numa for I/O
 *          NUMA        *numaRead()
 *          NUMA        *numaReadStream()
 *          l_int32      numaWrite()
 *          l_int32      numaWriteStream()
 *
 *      Numaa creation, destruction
 *          NUMAA       *numaaCreate()
 *          void        *numaaDestroy()
 *
 *      Add Numa to Numaa
 *          l_int32      numaaAddNuma()
 *          l_int32      numaaExtendArray()
 *
 *      Numaa accessors
 *          l_int32      numaaGetCount()
 *          l_int32      numaaGetNumaCount()
 *          l_int32      numaaGetNumberCount()
 *          NUMA       **numaaGetPtrArray()
 *          NUMA        *numaaGetNuma()
 *          NUMA        *numaaReplaceNuma()
 *          l_int32      numaaGetValue()
 *          l_int32      numaaAddNumber()
 *
 *      Serialize numaa for I/O
 *          NUMAA       *numaaRead()
 *          NUMAA       *numaaReadStream()
 *          l_int32      numaaWrite()
 *          l_int32      numaaWriteStream()
 *
 *      Numa2d creation, destruction
 *          NUMA2D      *numa2dCreate()
 *          void        *numa2dDestroy()
 *
 *      Numa2d Accessors
 *          l_int32      numa2dAddNumber()
 *          l_int32      numa2dGetCount()
 *          NUMA        *numa2dGetNuma()
 *          l_int32      numa2dGetFValue()
 *          l_int32      numa2dGetIValue()
 *
 *      NumaHash creation, destruction
 *          NUMAHASH    *numaHashCreate()
 *          void        *numaHashDestroy()
 *
 *      NumaHash Accessors
 *          NUMA        *numaHashGetNuma()
 *          void        *numaHashAdd()
 *
 *    (1) The Numa is a struct holding an array of floats.  It can also
 *        be used to store l_int32 values, with some loss of precision
 *        for floats larger than about 10 million.  Use the L_Dna instead
 *        if integers larger than a few million need to be stored.
 *
 *    (2) Always use the accessors in this file, never the fields directly.
 *
 *    (3) Storing and retrieving numbers:
 *
 *       * to append a new number to the array, use numaAddNumber().  If
 *         the number is an int, it will will automatically be converted
 *         to l_float32 and stored.
 *
 *       * to reset a value stored in the array, use numaSetValue().
 *
 *       * to increment or decrement a value stored in the array,
 *         use numaShiftValue().
 *
 *       * to obtain a value from the array, use either numaGetIValue()
 *         or numaGetFValue(), depending on whether you are retrieving
 *         an integer or a float.  This avoids doing an explicit cast,
 *         such as
 *           (a) return a l_float32 and cast it to an l_int32
 *           (b) cast the return directly to (l_float32 *) to
 *               satisfy the function prototype, as in
 *                 numaGetFValue(na, index, (l_float32 *)&ival);   [ugly!]
 *
 *    (4) int <--> float conversions:
 *
 *        Tradition dictates that type conversions go automatically from
 *        l_int32 --> l_float32, even though it is possible to lose
 *        precision for large integers, whereas you must cast (l_int32)
 *        to go from l_float32 --> l_int32 because you're truncating
 *        to the integer value.
 *
 *    (5) As with other arrays in leptonica, the numa has both an allocated
 *        size and a count of the stored numbers.  When you add a number, it
 *        goes on the end of the array, and causes a realloc if the array
 *        is already filled.  However, in situations where you want to
 *        add numbers randomly into an array, such as when you build a
 *        histogram, you must set the count of stored numbers in advance.
 *        This is done with numaSetCount().  If you set a count larger
 *        than the allocated array, it does a realloc to the size requested.
 *
 *    (6) In situations where the data in a numa correspond to a function
 *        y(x), the values can be either at equal spacings in x or at
 *        arbitrary spacings.  For the former, we can represent all x values
 *        by two parameters: startx (corresponding to y[0]) and delx
 *        for the change in x for adjacent values y[i] and y[i+1].
 *        startx and delx are initialized to 0.0 and 1.0, rsp.
 *        For arbitrary spacings, we use a second numa, and the two
 *        numas are typically denoted nay and nax.
 *
 *    (7) The numa is also the basic struct used for histograms.  Every numa
 *        has startx and delx fields, initialized to 0.0 and 1.0, that can
 *        be used to represent the "x" value for the location of the
 *        first bin and the bin width, respectively.  Accessors are the
 *        numa*XParameters() functions.  All functions that make numa
 *        histograms must set these fields properly, and many functions
 *        that use numa histograms rely on the correctness of these values.
 *}
procedure numaDestroy ( pna: PNumArray ); cdecl; external LIBLEPT;

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


