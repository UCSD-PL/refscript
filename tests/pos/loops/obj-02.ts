// related: tests/liquid/pos/loops/for-05.js

/*@ qualif Poo(v: int): v = -1 */
/*@ qualif Poo(v: int, i: int): v = i - 1 */
/*@ qualif Poo(v: int): v < 5 */


/*@ foo :: () => { (Immutable) a: { number | 4 = v } } */
function foo(): Object {

    let x = { a: -1 }; //need silly singleton v = -1 qualifier as we generate a template for this x. Ugh.

    for (let i = 0; i < 5; i++) {
        x = { a: i };
    }

    return x;
}
