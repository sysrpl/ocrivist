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
 *   pixabasic.c                                            PASCAL
 *
 *      Pixa creation, destruction, copying
 *           PIXA     *pixaCreate()
 *           PIXA     *pixaCreateFromPix()
 *           PIXA     *pixaCreateFromBoxa()
 *           PIXA     *pixaSplitPix()
 *           void      pixaDestroy()
 *           PIXA     *pixaCopy()
 *
 *      Pixa addition
 *           l_int32   pixaAddPix()
 *           l_int32   pixaExtendArray()
 *           l_int32   pixaExtendArrayToSize()
 *           l_int32   pixaAddBox()
 *
 *      Pixa accessors
 *           l_int32   pixaGetCount()                         X
 *           l_int32   pixaChangeRefcount()
 *           PIX      *pixaGetPix()
 *           l_int32   pixaGetPixDimensions()
 *           PIX      *pixaGetBoxa()
 *           l_int32   pixaGetBoxaCount()
 *           BOX      *pixaGetBox()
 *           l_int32   pixaGetBoxGeometry()
 *           PIX     **pixaGetPixArray()
 *
 *      Pixa array modifiers
 *           l_int32   pixaReplacePix()
 *           l_int32   pixaInsertPix()
 *           l_int32   pixaRemovePix()
 *           l_int32   pixaInitFull()
 *           l_int32   pixaClear()
 *
 *      Pixa combination
 *           PIXA     *pixaJoin()
 *
 *      Pixaa creation, destruction
 *           PIXAA    *pixaaCreate()
 *           PIXAA    *pixaaCreateFromPixa()
 *           void      pixaaDestroy()
 *
 *      Pixaa addition
 *           l_int32   pixaaAddPixa()
 *           l_int32   pixaaExtendArray()
 *           l_int32   pixaaAddBox()
 *
 *      Pixaa accessors
 *           l_int32   pixaaGetCount()
 *           PIXA     *pixaaGetPixa()
 *           BOXA     *pixaaGetBoxa()
 *           PIX      *pixaaGetPix()
 *
 *      Pixa serialized I/O  (requires png support)
 *           PIXA     *pixaRead()
 *           PIXA     *pixaReadStream()
 *           l_int32   pixaWrite()
 *           l_int32   pixaWriteStream()
 *
 *      Pixaa serialized I/O  (requires png support)
 *           PIXAA    *pixaaRead()
 *           PIXAA    *pixaaReadStream()
 *           l_int32   pixaaWrite()
 *           l_int32   pixaaWriteStream()
 *
 *
 *   Important note on reference counting:
 *     Reference counting for the Pixa is analogous to that for the Boxa.
 *     See pix.h for details.   pixaCopy() provides three possible modes
 *     of copy.  The basic rule is that however a Pixa is obtained
 *     (e.g., from pixaCreate*(), pixaCopy(), or a Pixaa accessor),
 *     it is necessary to call pixaDestroy() on it.
 *}

{*---------------------------------------------------------------------*
 *                             Pixa accessors                          *
 *---------------------------------------------------------------------*}

{*!
 *  pixaGetCount()
 *
 *      Input:  pixa
 *      Return: count, or 0 if no pixa
 *}
function pixaGetCount( pixa: PPixArray): Longint; cdecl; external LIBLEPT;


