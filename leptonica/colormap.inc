{*====================================================================*
 -  Copyright (C) 2001 Leptonica.  All rights reserved.
 *====================================================================*}

{*
 *  colormap.c                                                  PASCAL
 *
 *      Colormap creation, copy, destruction, addition
 *           PIXCMAP    *pixcmapCreate()
 *           PIXCMAP    *pixcmapCreateRandom()
 *           PIXCMAP    *pixcmapCreateLinear()
 *           PIXCMAP    *pixcmapCopy()
 *           void        pixcmapDestroy()
 *           l_int32     pixcmapAddColor()
 *           l_int32     pixcmapAddNewColor()
 *           l_int32     pixcmapAddNearestColor()
 *           l_int32     pixcmapUsableColor()
 *           l_int32     pixcmapAddBlackOrWhite()
 *           l_int32     pixcmapSetBlackAndWhite()
 *           l_int32     pixcmapGetCount()
 *           l_int32     pixcmapGetDepth()
 *           l_int32     pixcmapGetMinDepth()
 *           l_int32     pixcmapGetFreeCount()
 *           l_int32     pixcmapClear()
 *
 *      Colormap random access and test
 *           l_int32     pixcmapGetColor()
 *           l_int32     pixcmapGetColor32()
 *           l_int32     pixcmapResetColor()                      X
 *           l_int32     pixcmapGetIndex()
 *           l_int32     pixcmapHasColor()
 *           l_int32     pixcmapCountGrayColors()
 *           l_int32     pixcmapGetRankIntensity()
 *           l_int32     pixcmapGetNearestIndex()
 *           l_int32     pixcmapGetNearestGrayIndex()
 *           l_int32     pixcmapGetComponentRange()
 *           l_int32     pixcmapGetExtremeValue()
 *
 *      Colormap conversion
 *           PIXCMAP    *pixcmapGrayToColor()
 *           PIXCMAP    *pixcmapColorToGray()
 *
 *      Colormap I/O
 *           l_int32     pixcmapReadStream()
 *           l_int32     pixcmapWriteStream()
 *
 *      Extract colormap arrays and serialization
 *           l_int32     pixcmapToArrays()
 *           l_int32     pixcmapToRGBTable()
 *           l_int32     pixcmapSerializeToMemory()
 *           PIXCMAP    *pixcmapDeserializeFromMemory()
 *           char       *pixcmapConvertToHex()
 *
 *      Colormap transforms
 *           l_int32     pixcmapGammaTRC()
 *           l_int32     pixcmapContrastTRC()
 *           l_int32     pixcmapShiftIntensity()
 *           l_int32     pixcmapShiftByComponent()
 *}

{*!
 *  pixcmapResetColor()
 *
 *      Input:  cmap
 *              index
 *              rval, gval, bval (colormap entry to be reset; each number
 *                                is in range [0, ... 255])
 *      Return: 0 if OK, 1 if not accessable (caller should check)
 *
 *  Notes:
 *      (1) This resets sets the color of an entry that has already
 *          been set and included in the count of colors.
 *}
function pixcmapResetColor( cmap: PPixCmap; index, rval, gval, bval: Longint ): Longint; cdecl; external LIBLEPT;
