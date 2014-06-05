//  ===  binexpand.c  ==

{*------------------------------------------------------------------*
 *                      Power of 2 expansion                        *
 *------------------------------------------------------------------*/
 *!
 *  pixExpandBinaryPower2()
 *
 *      Input:  pixs (1 bpp)
 *              factor (expansion factor: 1, 2, 4, 8, 16)
 *      Return: pixd (expanded 1 bpp by replication), or null on error
 *}
function pixExpandBinaryPower2(pixs: PLPix; factor: longint): PLPix; cdecl; external LIBLEPT;
