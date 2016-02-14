

function idd<A>(x:A):A {
    return x;
}

function bar<A>(x: A, f: (y: A) => A): A {
    return f(x);
}

/*@ main :: () => { number | v > 5 } */
function main() {
    return bar(10, idd); // this is actually (idd @ number)
}
