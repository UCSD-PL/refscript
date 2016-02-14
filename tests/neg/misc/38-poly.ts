

function idd<A>(x:A):A {
    return x;
}

function bar<A>(x: A, f: (z:A) => A): A {
    return f(x);
}

/*@ main :: (x: { number | v > 5 }) => { number | v > 10 } */
function main(x){
    return bar(x, idd); // this is actually (idd @ number)
}
