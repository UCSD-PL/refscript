/*@ qualif PP(v: int): v > 1 */

/*@ type TT = {
    @Final f: { number | v > 1 };
           g: boolean;
} */
type TT = { f: number; g: boolean };

/*@ local bb :: TT */
let bb: TT = { f: 2, g: true };

bb.g = false;

let dd = bb;
assert(dd.f > 1);
