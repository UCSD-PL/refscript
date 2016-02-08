
/*@ foo :: () => { a: { number | v = 1 } } */
function foo(): Object {
    let x = { a: 1 };
    for (let i = 0; i < 5; i++) {
        x = { a: 2 };
    }
    return x;

}
