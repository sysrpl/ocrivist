{ This file is an automated translation to pascal of arithlow.c               }
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
 *  arithlow.c
 *
 *      One image grayscale arithmetic (8, 16 or 32 bpp)
 *            void       addConstantGrayLow()
 *            void       multConstantGrayLow()
 *
 *      Two image grayscale arithmetic (8, 16 or 32 bpp)
 *            void       addGrayLow()
 *            void       subtractGrayLow()
 *
 *      Grayscale threshold operation (8, 16 or 32 bpp)
 *            void       thresholdToValueLow()
 *
 *      Image accumulator arithmetic operations (8, 16, 32 bpp)
 *            void       finalAccumulateLow()
 *            void       finalAccumulateThreshLow()
 *            void       accumulateLow()
 *            void       multConstAccumulateLow()
 *
 *      Absolute value of difference, component-wise.
 *            void       absDifferenceLow()
 *}

{*------------------------------------------------------------------*
 *        One image grayscale arithmetic (8, 16 or 32 bpp)          *
 *------------------------------------------------------------------*}

{*!
 *  addConstantGrayLow()
 *}
procedure addConstantGrayLow( data: Pointer; w, h, d, wpl, val: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  multConstantGrayLow()
 *}
procedure multConstantGrayLow( data: Pointer; w, h, d, wpl: LongInt; val: Single ) ; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *        Two image grayscale arithmetic (8, 16 or 32 bpp)          *
 *------------------------------------------------------------------*}

{*!
 *  addGrayLow()
 *}
procedure addGrayLow( datad: Pointer; w, h, d, wpld: LongInt; datas: Pointer; wpls: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  subtractGrayLow()
 *}
procedure subtractGrayLow( datad: Pointer; w, h, d, wpld: LongInt; datas: Pointer; wpls: LongInt ) ; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                  Grayscale threshold operation              *
 *-------------------------------------------------------------*}

{*!
 *  thresholdToValueLow()
 *}
procedure thresholdToValueLow( datad: Pointer; w, h, d, wpld, threshval, setval: LongInt ) ; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *          Image accumulator arithmetic operations            *
 *-------------------------------------------------------------*}

{*!
 *  finalAccumulateLow()
 *}
procedure finalAccumulateLow( datad: Pointer; w, h, d, wpld: LongInt; datas: Pointer; wpls: LongInt; offset: LongWord ) ; cdecl; external LIBLEPT;

procedure finalAccumulateThreshLow( datad: Pointer; w, h, wpld: LongInt; datas: Pointer; wpls: LongInt; offset, threshold: LongWord ) ; cdecl; external LIBLEPT;


{*!
 *  accumulateLow()
 *}
procedure accumulateLow( datad: Pointer; w, h, wpld: LongInt; datas: Pointer; d, wpls, op: LongInt ) ; cdecl; external LIBLEPT;


{*!
 *  multConstAccumulateLow()
 *}
procedure multConstAccumulateLow( data: Pointer; w, h, wpl: LongInt; factor: Single; offset: LongWord ) ; cdecl; external LIBLEPT;


{*-----------------------------------------------------------------------*
 *              Absolute value of difference, component-wise             *
 *-----------------------------------------------------------------------*}

{*!
 *  absDifferenceLow()
 *
 *  Finds the absolute value of the difference of each pixel,
 *  for 8 and 16 bpp gray and for 32 bpp rgb.  For 32 bpp, the
 *  differences are found for each of the RGB components
 *  separately, and the LSB component is ignored.
 *  The results are written into datad.
 *}
procedure absDifferenceLow( datad: Pointer; w, h, wpld: LongInt; datas1, datas2: Pointer; d, wpls1, wpls2: LongInt ) ; cdecl; external LIBLEPT;


