
function twice<A>(f: (x: A) => A, x0: A): A {
    let x1 = f(x0);
    x1 = f(x1);
    return x1;
}

function foo(x: number): number {
    let z = 0;
    if (random() > 0) {
        z = 10;
    }
    return x + z;
}

/*@ main :: (n: number) => {v:number |v > n} */
function main(x) {
    return twice(foo, x);
}
