
/*@ foo :: (x: (Mutable) { a: number; b: string }) => { number | v > 5 } */

function foo(x) {

    for (let i = 0; i < 5; i++) {
        x.b = i;
    }

    return x.a;

}
