//  ===  pixarith.c  ===

{*-------------------------------------------------------------*
 *             Two-image grayscale arithmetic ops              *
 *-------------------------------------------------------------*}

{*!
 *  pixAddGray()
 *
 *      Input:  pixd (<optional>; this can be null, equal to pixs1, or
 *                    different from pixs1)
 *              pixs1 (can be == to pixd)
 *              pixs2
 *      Return: pixd always
 *
 *  Notes:
 *      (1) Arithmetic addition of two 8, 16 or 32 bpp images.
 *      (2) For 8 and 16 bpp, we do explicit clipping to 0xff and 0xffff,
 *          respectively.
 *      (3) Alignment is to UL corner.
 *      (4) There are 3 cases.  The result can go to a new dest,
 *          in-place to pixs1, or to an existing input dest:
 *          * pixd == null:   (src1 + src2) --> new pixd
 *          * pixd == pixs1:  (src1 + src2) --> src1  (in-place)
 *          * pixd != pixs1:  (src1 + src2) --> input pixd
 *      (5) pixs2 must be different from both pixd and pixs1.
 *}
function pixAddGray(pixd, pixs1, pixs2: PLPix): PLPix; cdecl; external LIBLEPT;



{*!
 *  pixSubtractGray()
 *
 *      Input:  pixd (<optional>; this can be null, equal to pixs1, or
 *                    different from pixs1)
 *              pixs1 (can be == to pixd)
 *              pixs2
 *      Return: pixd always
 *
 *  Notes:
 *      (1) Arithmetic subtraction of two 8, 16 or 32 bpp images.
 *      (2) Source pixs2 is always subtracted from source pixs1.
 *      (3) Do explicit clipping to 0.
 *      (4) Alignment is to UL corner.
 *      (5) There are 3 cases.  The result can go to a new dest,
 *          in-place to pixs1, or to an existing input dest:
 *          (a) pixd == null   (src1 - src2) --> new pixd
 *          (b) pixd == pixs1  (src1 - src2) --> src1  (in-place)
 *          (d) pixd != pixs1  (src1 - src2) --> input pixd
 *      (6) pixs2 must be different from both pixd and pixs1.
 *}
function pixSubtractGray(pixd, pixs1, pixs2: PLPix): PLPix; cdecl; external LIBLEPT;

{*-------------------------------------------------------------*
 *                Grayscale threshold operation                *
 *-------------------------------------------------------------*}

{*!
 *  pixThresholdToValue()
 *
 *      Input:  pixd (<optional>; if not null, must be equal to pixs)
 *              pixs (8, 16, 32 bpp)
 *              threshval
 *              setval
 *      Return: pixd always
 *
 *  Notes:
 *    - operation can be in-place (pixs == pixd) or to a new pixd
 *    - if setval > threshval, sets pixels with a value >= threshval to setval
 *    - if setval < threshval, sets pixels with a value <= threshval to setval
 *    - if setval == threshval, no-op
 *}
function pixThresholdToValue(pixd, pixs: PLPix; threshval, setval: longint): PLPix; cdecl; external LIBLEPT;

