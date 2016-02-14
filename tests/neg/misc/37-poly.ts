

function idd<A>(x:A):A {
    return x;
}

function bar<A>(x: A, f: (y: A) => A): A {
    return f(x);
}

/*@ main :: (number) => {string | 0 < 1} */
function main(x){
    return bar(x, idd); // this is actually (idd @ number)
}
