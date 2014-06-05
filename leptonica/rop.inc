//  === rop.c ===

{*--------------------------------------------------------------------*
 *                 Full image rasterop with no shifts                 *
 *--------------------------------------------------------------------*/
/*!
 *  pixRasteropFullImage()
 *
 *      Input:  pixd
 *              pixs
 *              op (any of the op-codes)
 *      Return: 0 if OK; 1 on error
 *
 *  Notes:
 *      - this is a wrapper for a common 2-image raster operation
 *      - both pixs and pixd must be defined
 *      - the operation is performed with aligned UL corners of pixs and pixd
 *      - the operation clips to the smallest pix; if the width or height
 *        of pixd is larger than pixs, some pixels in pixd will be unchanged
 *}
function pixRasteropFullImage(pixd, pixs: PLPix; op: longint): longint; cdecl; external LIBLEPT;


