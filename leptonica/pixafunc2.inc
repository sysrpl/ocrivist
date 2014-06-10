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
 *====================================================================* }

{*
 *   pixafunc2.c
 *
 *      Pixa Display (render into a pix)
 *           PIX      *pixaDisplay()
 *           PIX      *pixaDisplayOnColor()
 *           PIX      *pixaDisplayRandomCmap()
 *           PIX      *pixaDisplayOnLattice()
 *           PIX      *pixaDisplayUnsplit()
 *           PIX      *pixaDisplayTiled()
 *           PIX      *pixaDisplayTiledInRows()
 *           PIX      *pixaDisplayTiledAndScaled()
 *
 *      Pixaa Display (render into a pix)
 *           PIX      *pixaaDisplay()
 *           PIX      *pixaaDisplayByPixa()
 *           PIXA     *pixaaDisplayTiledAndScaled()
 *
 *  We give seven methods for displaying a pixa in a pix.
 *  Some work for 1 bpp input; others for any input depth.
 *  Some give an output depth that depends on the input depth;
 *  others give a different output depth or allow you to choose it.
 *  Some use a boxes to determine where each pix goes; others tile
 *  onto a regular lattice; yet others tile onto an irregular lattice.
 *
 *  Here is a brief description of what these functions do.
 *
 *    pixaDisplay()
 *        This uses the boxes to lay out each pix.  It is typically
 *        used to reconstruct a pix that has been broken into components.
 *    pixaDisplayOnColor()
 *        pixaDisplay() with choice of background color
 *    pixaDisplayRandomCmap()
 *        This also uses the boxes to lay out each pix.  However, it creates
 *        a colormapped dest, where each 1 bpp pix is given a randomly
 *        generated color (up to 256 are used).
 *    pixaDisplayOnLattice()
 *        This puts each pix, sequentially, onto a regular lattice,
 *        omitting any pix that are too big for the lattice size.
 *        This is useful, for example, to store bitmapped fonts,
 *        where all the characters are stored in a single image.
 *    pixaDisplayUnsplit()
 *        This lays out a mosaic of tiles (the pix in the pixa) that
 *        are all of equal size.  (Don't use this for unequal sized pix!)
 *        For example, it can be used to invert the action of
 *        pixaSplitPix().
 *    pixaDisplayTiled()
 *        Like pixaDisplayOnLattice(), this places each pix on a regular
 *        lattice, but here the lattice size is determined by the
 *        largest component, and no components are omitted.  This is
 *        dangerous if there are thousands of small components and
 *        one or more very large one, because the size of the resulting
 *        pix can be huge!
 *    pixaDisplayTiledInRows()
 *        This puts each pix down in a series of rows, where the upper
 *        edges of each pix in a row are alined and there is a uniform
 *        spacing between the pix.  The height of each row is determined
 *        by the tallest pix that was put in the row.  This function
 *        is a reasonably efficient way to pack the subimages.
 *    pixaDisplayTiledAndScaled()
 *        This scales each pix to a given width and output depth,
 *        and then tiles them in rows with a given number placed in
 *        each row.  This is very useful for presenting a sequence
 *        of images that can be at different resolutions, but which
 *        are derived from the same initial image.
 *}

 {*---------------------------------------------------------------------*
  *                               Pixa Display                          *
  *---------------------------------------------------------------------*}

 {*!
 *  pixaDisplayRandomCmap()
 *
 *      Input:  pixa (of 1 bpp components, with boxa)
 *              w, h (if set to 0, determines the size from the
 *                    b.b. of the components in pixa)
 *      Return: pix (8 bpp, cmapped, with random colors on the components),
 *              or null on error
 *
 *  Notes:
 *      (1) This uses the boxes to place each pix in the rendered composite.
 *      (2) By default, the background color is: black, cmap index 0.
 *          This can be changed by pixcmapResetColor()
 *}
function pixaDisplayRandomCmap( pixa: PPixArray; w, h: Longint ): PLPix; cdecl; external LIBLEPT;
