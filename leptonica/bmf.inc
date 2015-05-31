{ This file is an automated translation to pascal of bmf.c                   }
{ performed using the Leptonica Library Translator, written by Malcolm Poole }

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
 *  bmf.c
 *
 *   Acquisition and generation of bitmap fonts.
 *
 *       L_BMF           *bmfCreate()
 *       L_BMF           *bmfDestroy()
 *
 *       PIX             *bmfGetPix()
 *       l_int32          bmfGetWidth()
 *       l_int32          bmfGetBaseline()
 *
 *       PIXA            *pixaGetFont()
 *       l_int32          pixaSaveFont()
 *       PIXA            *pixaGenerateFont()
 *       static l_int32   pixGetTextBaseline()
 *       static l_int32   bmfMakeAsciiTables()
 *
 *   This is not a very general utility, because it only uses bitmap
 *   representations of a single font, Palatino-Roman, with the
 *   normal style.  It uses bitmaps generated for nine sizes, from
 *   4 to 20 pts, rendered at 300 ppi.  Generalization to different
 *   fonts, styles and sizes is straightforward.
 *
 *   I chose Palatino-Roman is because I like it.
 *   The input font images were generated from a set of small
 *   PostScript files, such as chars-12.ps, which were rendered
 *   into the inputfont[] bitmap files using GhostScript.  See, for
 *   example, the bash script prog/ps2tiff, which will "rip" a
 *   PostScript file into a set of ccitt-g4 compressed tiff files.
 *
 *   The set of ascii characters from 32 through 126 are the 95
 *   printable ascii chars.  Palatino-Roman is missing char 92, '\'.
 *   I have substituted '/', char 47, for 92, so that there will be
 *   no missing printable chars in this set.  The space is char 32,
 *   and I have given it a width equal to twice the width of '!'.
 *}

{*---------------------------------------------------------------------*/
/*                           Bmf create/destroy                        */
/*---------------------------------------------------------------------*}
{*!
 *  bmfCreate()
 *
 *      Input:  dir (directory holding pixa of character set)
 *              size (4, 6, 8, ... , 20)
 *      Return: bmf (holding the bitmap font and associated information)
 *
 *  Notes:
 *      (1) This first tries to read a pre-computed pixa file with the
 *          95 ascii chars in it.  If the file is not found, it
 *          creates the pixa from the raw image.  It then generates all
 *          associated data required to use the bmf.
 *}
function bmfCreate(dir: PChar; size: LongInt): PBmFont; cdecl;  external LIBLEPT;

{*!
 *  bmfDestroy()
 *
 *      Input:  &bmf (<set to null>)
 *}
procedure bmfDestroy(pbmf: PPBmFont); cdecl;  external LIBLEPT;




