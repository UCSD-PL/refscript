
/*@ qualif Ineq(v : int): (v <= 6) */

/*@ orUndef :: <A>(x: A) => A + undefined */
declare function orUndef<A>(x: A): A;

/*@ loop :: () => { number | v = 6 } */
function loop() {
    /*@ local x :: number + undefined */
    let x = 1;
    while (x <= 5) {
        x = x + 1;
    }
    return x;
}
