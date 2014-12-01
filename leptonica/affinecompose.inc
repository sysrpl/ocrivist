{ This file is an automated translation to pascal of affinecompose.c          }
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
 *  affinecompose.c
 *
 *      Composable coordinate transforms
 *           l_float32   *createMatrix2dTranslate()
 *           l_float32   *createMatrixScale()
 *           l_float32   *createMatrixRotate()
 *
 *      Special coordinate transforms on pta
 *           PTA         *ptaTranslate()
 *           PTA         *ptaScale()
 *           PTA         *ptaRotate()
 *
 *      Special coordinate transforms on boxa
 *           BOXA        *boxaTranslate()
 *           BOXA        *boxaScale()
 *           BOXA        *boxaRotate()
 *
 *      General coordinate transform on pta and boxa
 *           PTA         *ptaAffineTransform()
 *           BOXA        *boxaAffineTransform()
 *
 *      Matrix operations
 *           l_int32      l_productMatVec()
 *           l_int32      l_productMat2()
 *           l_int32      l_productMat3()
 *           l_int32      l_productMat4()
 *}

{*-------------------------------------------------------------*
 *                Composable coordinate transforms             *
 *-------------------------------------------------------------*}

{*!
 *  createMatrix2dTranslate()
 *
 *      Input:  transx  (x component of translation wrt. the origin)
 *              transy  (y component of translation wrt. the origin)
 *      Return: 3x3 transform matrix, or null on error
 *
 *  Notes;
 *      (1) The translation is equivalent to:
 *             v' = Av
 *          where v and v' are 1x3 column vectors in the form
 *             v = [x, y, 1]^    (^ denotes transpose)
 *          and the affine tranlation matrix is
 *             A = [ 1   0   tx
 *                   0   1   ty
 *                   0   0    1  ]
 *
 *      (2) We consider translation as with respect to a fixed origin.
 *          In a clipping operation, the origin moves and the points
 *          are fixed, and you use (-tx, -ty) where (tx, ty) is the
 *          translation vector of the origin.
 *}
function createMatrix2dTranslate( transx, transy: Single ) : PSingle; cdecl; external LIBLEPT;


{*!
 *  createMatrix2dScale()
 *
 *      Input:  scalex  (horizontal scale factor)
 *              scaley  (vertical scale factor)
 *      Return: 3x3 transform matrix, or null on error
 *
 *  Notes;
 *      (1) The scaling is equivalent to:
 *             v' = Av
 *          where v and v' are 1x3 column vectors in the form
 *             v = [x, y, 1]^    (^ denotes transpose)
 *          and the affine scaling matrix is
 *             A = [ sx  0    0
 *                   0   sy   0
 *                   0   0    1  ]
 *
 *      (2) We consider scaling as with respect to a fixed origin.
 *          In other words, the origin is the only point that doesn't
 *          move in the scaling transform.
 *}
function createMatrix2dScale( scalex, scaley: Single ) : PSingle; cdecl; external LIBLEPT;


{*!
 *  createMatrix2dRotate()
 *
 *      Input:  xc, yc  (location of center of rotation)
 *              angle  (rotation in radians; clockwise is positive)
 *      Return: 3x3 transform matrix, or null on error
 *
 *  Notes;
 *      (1) The rotation is equivalent to:
 *             v' = Av
 *          where v and v' are 1x3 column vectors in the form
 *             v = [x, y, 1]^    (^ denotes transpose)
 *          and the affine rotation matrix is
 *             A = [ cosa   -sina    xc*(1-cosa) + yc*sina
 *                   sina    cosa    yc*(1-cosa) - xc*sina
 *                     0       0                 1         ]
 *
 *          If the rotation is about the origin, (xc, yc) = (0, 0) and
 *          this simplifies to
 *             A = [ cosa   -sina    0
 *                   sina    cosa    0
 *                     0       0     1 ]
 *
 *          These relations follow from the following equations, which
 *          you can convince yourself are correct as follows.  Draw a
 *          circle centered on (xc,yc) and passing through (x,y), with
 *          (x',y') on the arc at an angle 'a' clockwise from (x,y).
 *          [ Hint: cos(a + b) = cosa * cosb - sina * sinb
 *                  sin(a + b) = sina * cosb + cosa * sinb ]
 *
 *            x' - xc =  (x - xc) * cosa - (y - yc) * sina
 *            y' - yc =  (x - xc) * sina + (y - yc) * cosa
 *}
function createMatrix2dRotate( xc, yc, angle: Single ) : PSingle; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *            Special coordinate transforms on pta             *
 *-------------------------------------------------------------*}

{*!
 *  ptaTranslate()
 *
 *      Input:  ptas (for initial points)
 *              transx  (x component of translation wrt. the origin)
 *              transy  (y component of translation wrt. the origin)
 *      Return: ptad  (translated points), or null on error
 *
 *  Notes;
 *      (1) See createMatrix2dTranslate() for details of transform.
 *}
function ptaTranslate( ptas: PPointArray; transx, transy: Single ) : PPointArray; cdecl; external LIBLEPT;


{*!
 *  ptaScale()
 *
 *      Input:  ptas (for initial points)
 *              scalex  (horizontal scale factor)
 *              scaley  (vertical scale factor)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes;
 *      (1) See createMatrix2dScale() for details of transform.
 *}
function ptaScale( ptas: PPointArray; scalex, scaley: Single ) : PPointArray; cdecl; external LIBLEPT;


{*!
 *  ptaRotate()
 *
 *      Input:  ptas (for initial points)
 *              (xc, yc)  (location of center of rotation)
 *              angle  (rotation in radians; clockwise is positive)
 *              (&ptad)  (<return> new locations)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes;
 *      (1) See createMatrix2dScale() for details of transform.
 *}
function ptaRotate( ptas: PPointArray; xc, yc, angle: Single ) : PPointArray; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *            Special coordinate transforms on boxa            *
 *-------------------------------------------------------------*}

{*!
 *  boxaTranslate()
 *
 *      Input:  boxas
 *              transx  (x component of translation wrt. the origin)
 *              transy  (y component of translation wrt. the origin)
 *      Return: boxad  (translated boxas), or null on error
 *
 *  Notes;
 *      (1) See createMatrix2dTranslate() for details of transform.
 *}
function boxaTranslate( boxas: PBoxArray; transx, transy: Single ) : PBoxArray; cdecl; external LIBLEPT;


{*!
 *  boxaScale()
 *
 *      Input:  boxas
 *              scalex  (horizontal scale factor)
 *              scaley  (vertical scale factor)
 *      Return: boxad  (scaled boxas), or null on error
 *
 *  Notes;
 *      (1) See createMatrix2dScale() for details of transform.
 *}
function boxaScale( boxas: PBoxArray; scalex, scaley: Single ) : PBoxArray; cdecl; external LIBLEPT;


{*!
 *  boxaRotate()
 *
 *      Input:  boxas
 *              (xc, yc)  (location of center of rotation)
 *              angle  (rotation in radians; clockwise is positive)
 *      Return: boxad  (scaled boxas), or null on error
 *
 *  Notes;
 *      (1) See createMatrix2dRotate() for details of transform.
 *}
function boxaRotate( boxas: PBoxArray; xc, yc, angle: Single ) : PBoxArray; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *            General affine coordinate transform              *
 *-------------------------------------------------------------*}

{*!
 *  ptaAffineTransform()
 *
 *      Input:  ptas (for initial points)
 *              mat  (3x3 transform matrix; canonical form)
 *      Return: ptad  (transformed points), or null on error
 *}
function ptaAffineTransform( ptas: PPointArray; mat: PSingle ) : PPointArray; cdecl; external LIBLEPT;


{*!
 *  boxaAffineTransform()
 *
 *      Input:  boxas
 *              mat  (3x3 transform matrix; canonical form)
 *      Return: boxad  (transformed boxas), or null on error
 *}
function boxaAffineTransform( boxas: PBoxArray; mat: PSingle ) : PBoxArray; cdecl; external LIBLEPT;


{*-------------------------------------------------------------*
 *                      Matrix operations                      *
 *-------------------------------------------------------------*}

{*!
 *  l_productMatVec()
 *
 *      Input:  mat  (square matrix, as a 1-dimensional @size^2 array)
 *              vecs (input column vector of length @size)
 *              vecd (result column vector)
 *              size (matrix is @size x @size; vectors are length @size)
 *      Return: 0 if OK, 1 on error
 *}
function l_productMatVec( mat, vecs, vecd: PSingle; size: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_productMat2()
 *
 *      Input:  mat1  (square matrix, as a 1-dimensional size^2 array)
 *              mat2  (square matrix, as a 1-dimensional size^2 array)
 *              matd  (square matrix; product stored here)
 *              size (of matrices)
 *      Return: 0 if OK, 1 on error
 *}
function l_productMat2( mat1, mat2, matd: PSingle; size: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_productMat3()
 *
 *      Input:  mat1  (square matrix, as a 1-dimensional size^2 array)
 *              mat2  (square matrix, as a 1-dimensional size^2 array)
 *              mat3  (square matrix, as a 1-dimensional size^2 array)
 *              matd  (square matrix; product stored here)
 *              size  (of matrices)
 *      Return: 0 if OK, 1 on error
 *}
function l_productMat3( mat1, mat2, mat3, matd: PSingle; size: LongInt ) : LongInt; cdecl; external LIBLEPT;


{*!
 *  l_productMat4()
 *
 *      Input:  mat1  (square matrix, as a 1-dimensional size^2 array)
 *              mat2  (square matrix, as a 1-dimensional size^2 array)
 *              mat3  (square matrix, as a 1-dimensional size^2 array)
 *              mat4  (square matrix, as a 1-dimensional size^2 array)
 *              matd  (square matrix; product stored here)
 *              size  (of matrices)
 *      Return: 0 if OK, 1 on error
 *}
function l_productMat4( mat1, mat2, mat3, mat4, matd: PSingle; size: LongInt ) : LongInt; cdecl; external LIBLEPT;


