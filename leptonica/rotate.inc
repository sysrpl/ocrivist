{ This file is an automated translation to pascal of rotate.c                 }
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
 *  rotate.c
 *
 *     General rotation about image center
 *              PIX     *pixRotate()
 *              PIX     *pixEmbedForRotation()
 *
 *     General rotation by sampling
 *              PIX     *pixRotateBySampling()
 *
 *     Nice (slow) rotation of 1 bpp image
 *              PIX     *pixRotateBinaryNice()
 *
 *     Rotation including alpha (blend) component and gamma transform
 *              PIX     *pixRotateWithAlpha()
 *              PIX     *pixRotateGammaXform()
 *
 *     Rotations are measured in radians; clockwise is positive.
 *
 *     The general rotation pixRotate() does the best job for
 *     rotating about the image center.  For 1 bpp, it uses shear;
 *     for others, it uses either shear or area mapping.
 *     If requested, it expands the output image so that no pixels are lost
 *     in the rotation, and this can be done on multiple successive shears
 *     without expanding beyond the maximum necessary size.
 *}
const VERY_SMALL_ANGLE : Single = 0.001;     // radians; ~0.06 degrees 

{*------------------------------------------------------------------*
 *                  General rotation about the center               *
 *------------------------------------------------------------------*}

{*!
 *  pixRotate()
 *
 *      Input:  pixs (1, 2, 4, 8, 32 bpp rgb)
 *              angle (radians; clockwise is positive)
 *              type (L_ROTATE_AREA_MAP, L_ROTATE_SHEAR, L_ROTATE_SAMPLING)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *              width (original width; use 0 to avoid embedding)
 *              height (original height; use 0 to avoid embedding)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) Rotation is about the center of the image.
 *      (2) For very small rotations, just return a clone.
 *      (3) Rotation brings either white or black pixels in
 *          from outside the image.
 *      (4) Above 20 degrees, if rotation by shear is requested, we rotate
 *          by sampling.
 *      (5) Colormaps are removed for rotation by area map and shear.
 *      (6) The dest can be expanded so that no image pixels
 *          are lost.  To invoke expansion, input the original
 *          width and height.  For repeated rotation, use of the
 *          original width and height allows the expansion to
 *          stop at the maximum required size, which is a square 
 *          with side = sqrt(w*w + h*h).
 *
 *  *** Warning: implicit assumption about RGB component ordering ***
 *}
function pixRotate( pixs: PLPix; angle: Single; L_type, incolor, width, height: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixEmbedForRotation()
 *
 *      Input:  pixs (1, 2, 4, 8, 32 bpp rgb)
 *              angle (radians; clockwise is positive)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *              width (original width; use 0 to avoid embedding)
 *              height (original height; use 0 to avoid embedding)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) For very small rotations, just return a clone.
 *      (2) Generate larger image to embed pixs if necessary, and
 *          place in the center.
 *      (3) Rotation brings either white or black pixels in
 *          from outside the image.  For colormapped images where
 *          there is no white or black, a new color is added if
 *          possible for these pixels; otherwise, either the
 *          lightest or darkest color is used.  In most cases,
 *          the colormap will be removed prior to rotation.
 *      (4) The dest is to be expanded so that no image pixels
 *          are lost after rotation.  Input of the original width
 *          and height allows the expansion to stop at the maximum
 *          required size, which is a square with side equal to
 *          sqrt(w*w + h*h).
 *      (5) For an arbitrary angle, the expansion can be found by
 *          considering the UL and UR corners.  As the image is
 *          rotated, these move in an arc centered at the center of
 *          the image.  Normalize to a unit circle by dividing by half
 *          the image diagonal.  After a rotation of T radians, the UL
 *          and UR corners are at points T radians along the unit
 *          circle.  Compute the x and y coordinates of both these
 *          points and take the max of absolute values; these represent
 *          the half width and half height of the containing rectangle.
 *          The arithmetic is done using formulas for sin(a+b) and cos(a+b),
 *          where b = T.  For the UR corner, sin(a) = h/d and cos(a) = w/d.
 *          For the UL corner, replace a by (pi - a), and you have
 *          sin(pi - a) = h/d, cos(pi - a) = -w/d.  The equations
 *          given below follow directly.
 *}
function pixEmbedForRotation( pixs: PLPix; angle: Single; incolor, width, height: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                    General rotation by sampling                  *
 *------------------------------------------------------------------*}

{*!
 *  pixRotateBySampling()
 *
 *      Input:  pixs (1, 2, 4, 8, 16, 32 bpp rgb; can be cmapped)
 *              xcen (x value of center of rotation)
 *              ycen (y value of center of rotation)
 *              angle (radians; clockwise is positive)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) For very small rotations, just return a clone.
 *      (2) Rotation brings either white or black pixels in
 *          from outside the image.
 *      (3) Colormaps are retained.
 *}
function pixRotateBySampling( pixs: PLPix; xcen, ycen: LongInt; angle: Single; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *                 Nice (slow) rotation of 1 bpp image              *
 *------------------------------------------------------------------*}

{*!
 *  pixRotateBinaryNice()
 *
 *      Input:  pixs (1 bpp)
 *              angle (radians; clockwise is positive; about the center)
 *              incolor (L_BRING_IN_WHITE, L_BRING_IN_BLACK)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) For very small rotations, just return a clone.
 *      (2) This does a computationally expensive rotation of 1 bpp images.
 *          The fastest rotators (using shears or subsampling) leave
 *          visible horizontal and vertical shear lines across which
 *          the image shear changes by one pixel.  To ameliorate the
 *          visual effect one can introduce random dithering.  One
 *          way to do this in a not-too-random fashion is given here.
 *          We convert to 8 bpp, do a very small blur, rotate using
 *          linear interpolation (same as area mapping), do a
 *          small amount of sharpening to compensate for the initial
 *          blur, and threshold back to binary.  The shear lines
 *          are magically removed.
 *      (3) This operation is about 5x slower than rotation by sampling.
 *}
function pixRotateBinaryNice( pixs: PLPix; angle: Single; incolor: LongInt ) : PLPix; cdecl; external LIBLEPT;


{*------------------------------------------------------------------*
 *             Rotation including alpha (blend) component           *
 *------------------------------------------------------------------*}

{*!
 *  pixRotateWithAlpha()
 *
 *      Input:  pixs (32 bpp rgb)
 *              angle (radians; clockwise is positive)
 *              pixg (<optional> 8 bpp, can be null)
 *              fract (between 0.0 and 1.0, with 0.0 fully transparent
 *                     and 1.0 fully opaque)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) The alpha channel is transformed separately from pixs,
 *          and aligns with it, being fully transparent outside the
 *          boundary of the transformed pixs.  For pixels that are fully
 *          transparent, a blending function like pixBlendWithGrayMask()
 *          will give zero weight to corresponding pixels in pixs.
 *      (2) Rotation is about the center of the image; for very small
 *          rotations, just return a clone.  The dest is automatically
 *          expanded so that no image pixels are lost.
 *      (3) Rotation is by area mapping.  It doesn't matter what
 *          color is brought in because the alpha channel will
 *          be transparent (black) there.
 *      (4) If pixg is NULL, it is generated as an alpha layer that is
 *          partially opaque, using @fract.  Otherwise, it is cropped
 *          to pixs if required and @fract is ignored.  The alpha
 *          channel in pixs is never used.
 *      (4) Colormaps are removed.
 *      (5) The default setting for the border values in the alpha channel
 *          is 0 (transparent) for the outermost ring of pixels and
 *          (0.5 * fract * 255) for the second ring.  When blended over
 *          a second image, this
 *          (a) shrinks the visible image to make a clean overlap edge
 *              with an image below, and
 *          (b) softens the edges by weakening the aliasing there.
 *          Use l_setAlphaMaskBorder() to change these values.
 *
 *  *** Warning: implicit assumption about RGB component ordering ***
 *}
function pixRotateWithAlpha( pixs: PLPix; angle: Single; pixg: PLPix; fract: Single ) : PLPix; cdecl; external LIBLEPT;


{*!
 *  pixRotateGammaXform()
 *
 *      Input:  pixs (32 bpp rgb)
 *              gamma (gamma correction; must be > 0.0)
 *              angle (radians; clockwise is positive)
 *              fract (between 0.0 and 1.0, with 1.0 fully transparent)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This wraps a gamma/inverse-gamma photometric transform
 *          around pixRotateWithAlpha().
 *      (2) For usage, see notes in pixRotateWithAlpha() and
 *          pixGammaTRCWithAlpha().
 *      (3) The basic idea of a gamma/inverse-gamma transform is
 *          to remove gamma correction before rotating and restore
 *          it afterward.  The effects can be subtle, but important for
 *          some applications.  For example, using gamma > 1.0 will
 *          cause the dark areas to become somewhat lighter and slightly
 *          reduce aliasing effects when blending using the alpha channel.
 *}
function pixRotateGammaXform( pixs: PLPix; gamma, angle, fract: Single ) : PLPix; cdecl; external LIBLEPT;


