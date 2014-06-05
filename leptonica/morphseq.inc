//  === morphseq.c  ===
{*-------------------------------------------------------------------------*
 *         Run a sequence of binary rasterop morphological operations      *
 *-------------------------------------------------------------------------*}

{*!
 *  pixMorphSequence()
 *
 *      Input:  pixs
 *              sequence (string specifying sequence)
 *              dispsep (controls debug display of each result in the sequence:
 *                       0: no output
 *                       > 0: gives horizontal separation in pixels between
 *                            successive displays
 *                       < 0: pdf output; abs(dispsep) is used for naming)
 *      Return: pixd, or null on error
 *
 *  Notes:
 *      (1) This does rasterop morphology on binary images.
 *      (2) This runs a pipeline of operations; no branching is allowed.
 *      (3) This only uses brick Sels, which are created on the fly.
 *          In the future this will be generalized to extract Sels from
 *          a Sela by name.
 *      (4) A new image is always produced; the input image is not changed.
 *      (5) This contains an interpreter, allowing sequences to be
 *          generated and run.
 *      (6) The format of the sequence string is defined below.
 *      (7) In addition to morphological operations, rank order reduction
 *          and replicated expansion allow operations to take place
 *          downscaled by a power of 2.
 *      (8) Intermediate results can optionally be displayed.
 *      (9) Thanks to Dar-Shyang Lee, who had the idea for this and
 *          built the first implementation.
 *      (10) The sequence string is formatted as follows:
 *            - An arbitrary number of operations,  each separated
 *              by a '+' character.  White space is ignored.
 *            - Each operation begins with a case-independent character
 *              specifying the operation:
 *                 d or D  (dilation)
 *                 e or E  (erosion)
 *                 o or O  (opening)
 *                 c or C  (closing)
 *                 r or R  (rank binary reduction)
 *                 x or X  (replicative binary expansion)
 *                 b or B  (add a border of 0 pixels of this size)
 *            - The args to the morphological operations are bricks of hits,
 *              and are formatted as a.b, where a and b are horizontal and
 *              vertical dimensions, rsp.
 *            - The args to the reduction are a sequence of up to 4 integers,
 *              each from 1 to 4.
 *            - The arg to the expansion is a power of two, in the set
 *              [2, 4, 8, 16].
 *      (11) An example valid sequence is:
 *               "b32 + o1.3 + C3.1 + r23 + e2.2 + D3.2 + X4"
 *           In this example, the following operation sequence is carried out:
 *             * b32: Add a 32 pixel border around the input image
 *             * o1.3: Opening with vert sel of length 3 (e.g., 1 x 3)
 *             * C3.1: Closing with horiz sel of length 3  (e.g., 3 x 1)
 *             * r23: Two successive 2x2 reductions with rank 2 in the first
 *                    and rank 3 in the second.  The result is a 4x reduced pix.
 *             * e2.2: Erosion with a 2x2 sel (origin will be at x,y: 0,0)
 *             * d3.2: Dilation with a 3x2 sel (origin will be at x,y: 1,0)
 *             * X4: 4x replicative expansion, back to original resolution
 *      (12) The safe closing is used.  However, if you implement a
 *           closing as separable dilations followed by separable erosions,
 *           it will not be safe.  For that situation, you need to add
 *           a sufficiently large border as the first operation in
 *           the sequence.  This will be removed automatically at the
 *           end.  There are two cautions:
 *              - When computing what is sufficient, remember that if
 *                reductions are carried out, the border is also reduced.
 *              - The border is removed at the end, so if a border is
 *                added at the beginning, the result must be at the
 *                same resolution as the input!
 *}
function pixMorphSequence(pixs: PLPix; sequence: PChar; dispsep: longint): PLPix;  cdecl; external LIBLEPT;   
