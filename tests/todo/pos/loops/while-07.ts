
/*@ qualif Ineq(v : int): (v <= 6) */


// XXX: uncommmenting the following fixes the issue

// /* @ local y :: number + undefined */
// let y = 1;
// let z = <number>y + 1;
// assert(z === 2);


/*@ loop :: () => { number | v = 6 } */
function loop() {
    /*@ local x :: number + undefined */
    let x = 1;
    while (<number>x <= 5) {
        x = <number>x + 1;
    }
    return x;
}
