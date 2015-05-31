{ This file is an automated translation to pascal of rotateorth.c             }
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
 *  rotateorth.c
 *
 *      Top-level rotation by multiples of 90 degrees
 *            PIX     *pixRotateOrth()
 *
 *      180-degree rotation
 *            PIX     *pixRotate180()
 *
 *      90-degree rotation (both directions)
 *            PIX     *pixRotate90()
 *
 *      Left-right flip
 *            PIX     *pixFlipLR()
 *
 *      Top-bottom flip
 *            PIX     *pixFlipTB()
 *}

{*!
 *  pixRotateOrth()
 *
 *      Input:  pixs (all depths)
 *              quads (0-3; number of 90 degree cw rotations)
 *      Return: pixd, or null on error
 *}
function pixRotateOrth( pixs: PLPix; quads: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRotate180()
 *
 *      Input:  pixd  (<optional>; can be null, equal to pixs,
 *                     or different from pixs)
 *              pixs (all depths)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does a 180 rotation of the image about the center,
 *          which is equivalent to a left-right flip about a vertical
 *          line through the image center, followed by a top-bottom
 *          flip about a horizontal line through the image center.
 *      (2) There are 3 cases for input:
 *          (a) pixd == null (creates a new pixd)
 *          (b) pixd == pixs (in-place operation)
 *          (c) pixd != pixs (existing pixd)
 *      (3) For clarity, use these three patterns, respectively:
 *          (a) pixd = pixRotate180(NULL, pixs);
 *          (b) pixRotate180(pixs, pixs);
 *          (c) pixRotate180(pixd, pixs);
 *}
function pixRotate180( pixd, pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRotate90()
 *
 *      Input:  pixs (all depths)
 *              direction (1 = clockwise,  -1 = counter-clockwise)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does a 90 degree rotation of the image about the center,
 *          either cw or ccw, returning a new pix.
 *      (2) The direction must be either 1 (cw) or -1 (ccw).
 *}
function pixRotate90( pixs: PLPix; direction: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixFlipLR()
 *
 *      Input:  pixd  (<optional>; can be null, equal to pixs,
 *                     or different from pixs)
 *              pixs (all depths)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does a left-right flip of the image, which is
 *          equivalent to a rotation out of the plane about a
 *          vertical line through the image center.
 *      (2) There are 3 cases for input:
 *          (a) pixd == null (creates a new pixd)
 *          (b) pixd == pixs (in-place operation)
 *          (c) pixd != pixs (existing pixd)
 *      (3) For clarity, use these three patterns, respectively:
 *          (a) pixd = pixFlipLR(NULL, pixs);
 *          (b) pixFlipLR(pixs, pixs);
 *          (c) pixFlipLR(pixd, pixs);
 *      (4) If an existing pixd is not the same size as pixs, the
 *          image data will be reallocated.
 *}
function pixFlipLR( pixd, pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixFlipTB()
 *
 *      Input:  pixd  (<optional>; can be null, equal to pixs,
 *                     or different from pixs)
 *              pixs (all depths)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does a top-bottom flip of the image, which is
 *          equivalent to a rotation out of the plane about a
 *          horizontal line through the image center.
 *      (2) There are 3 cases for input:
 *          (a) pixd == null (creates a new pixd)
 *          (b) pixd == pixs (in-place operation)
 *          (c) pixd != pixs (existing pixd)
 *      (3) For clarity, use these three patterns, respectively:
 *          (a) pixd = pixFlipTB(NULL, pixs);
 *          (b) pixFlipTB(pixs, pixs);
 *          (c) pixFlipTB(pixd, pixs);
 *      (4) If an existing pixd is not the same size as pixs, the
 *          image data will be reallocated.
 *}
function pixFlipTB( pixd, pixs: PLPix ) : PLPix; cdecl; external LIBLEPT;


