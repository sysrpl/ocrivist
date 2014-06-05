//  ===  graymorph.c  ===

{*-----------------------------------------------------------------*
 *              Top-level gray morphological operations            *
 *-----------------------------------------------------------------*}

 {*!
 *  pixErodeGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixErodeGray(pixs: PLPix; hsize, vsize: longint): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixDilateGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixDilateGray(pixs: PLPix; hsize, vsize: longint): PLPix; cdecl; external LIBLEPT;

{*!
 *  pixOpenGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixOpenGray(pixs: PLPix; hsize, vsize: longint): PLPix; cdecl; external LIBLEPT;


{*!
 *  pixCloseGray()
 *
 *      Input:  pixs
 *              hsize  (of Sel; must be odd; origin implicitly in center)
 *              vsize  (ditto)
 *      Return: pixd
 *
 *  Notes:
 *      (1) Sel is a brick with all elements being hits
 *      (2) If hsize = vsize = 1, just returns a copy.
 *}
function pixCloseGray(pixs: PLPix; hsize, vsize: Longint): PLPix;  cdecl; external LIBLEPT;

