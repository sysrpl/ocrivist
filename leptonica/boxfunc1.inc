//  ===  boxfunc1.c  ===

{*---------------------------------------------------------------------*
 *                             Box geometry                            *
 *---------------------------------------------------------------------*/}

{*!
 *  boxContains()
 *
 *      Input:  box1, box2
 *              &result (<return> 1 if box2 is entirely contained within
 *                       box1, and 0 otherwise)
 *      Return: 0 if OK, 1 on error
 *}
function boxContains(box1, box2: PLBox; presult: PLongint): longint; cdecl; external LIBLEPT;


{!
 *  boxIntersects()
 *
 *      Input:  box1, box2
 *              &result (<return> 1 if any part of box2 is contained
 *                      in box1, and 0 otherwise)
 *      Return: 0 if OK, 1 on error
 *}
function boxIntersects(box1, box2: PLBox; presult: PLongint): longint; cdecl; external LIBLEPT;

{*!
 *  boxaContainedInBox()
 *
 *      Input:  boxas
 *              box (for containment)
 *      Return: boxad (boxa with all boxes in boxas that are
 *                     entirely contained in box), or null on error
 *
 *  Notes:
 *      (1) All boxes in boxa that are entirely outside box are removed.
 *}
function boxaContainedInBox(boxas: PBoxArray; box: PLBox): PBoxArray; cdecl; external LIBLEPT;


