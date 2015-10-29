/*@ option REALS */

/*@ type nat = {number | v >= 0} */
type nat = number;

/*@ foo :: (x:nat, y:nat) => {void | true} */
export function foo(x, y) {
    assert((x + 1) + (y + 1) * (x + 2) < (x + 2) * (y + 2));
}
