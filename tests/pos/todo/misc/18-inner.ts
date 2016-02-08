/*@ qualif Equal(v:int, x: int, y: int) : v = x + y */

/*@ main :: (xx: number) => { v: number |v > xx} */
function main(xx: number): number {

    /*@ readonly */
    let x = xx;


    // XXX: how about without this?
    /*@ plus :: (a: number) => { number | v = a + x } */
    function plus(a: number): number {
        return a + x;
    };

    let z = plus(12);

    return z;
}
