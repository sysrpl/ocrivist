//  === seedfill.c  ===

{*!
 *  pixSeedfillBinary()
 *
 *      Input:  pixd  (<optional>; this can be null, equal to pixs,
 *                     or different from pixs; 1 bpp)
 *              pixs  (1 bpp seed)
 *              pixm  (1 bpp filling mask)
 *              connectivity  (4 or 8)
 *      Return: pixd always
 *
 *  Notes:
 *      (1) This is for binary seedfill (aka "binary reconstruction").
 *      (2) There are 3 cases:
 *            (a) pixd == null (make a new pixd)
 *            (b) pixd == pixs (in-place)
 *            (c) pixd != pixs
 *      (3) If you know the case, use these patterns for clarity:
 *            (a) pixd = pixSeedfillBinary(NULL, pixs, ...);
 *            (b) pixSeedfillBinary(pixs, pixs, ...);
 *            (c) pixSeedfillBinary(pixd, pixs, ...);
 *      (4) The resulting pixd contains the filled seed.  For some
 *          applications you want to OR it with the inverse of
 *          the filling mask.
 *      (5) The input seed and mask images can be different sizes, but
 *          in typical use the difference, if any, would be only
 *          a few pixels in each direction.  If the sizes differ,
 *          the clipping is handled by the low-level function
 *          seedfillBinaryLow().
 *}
function pixSeedfillBinary(pixd, pixs, pixm: PLPix; connectivity: longint): PLPix; cdecl; external LIBLEPT;  
