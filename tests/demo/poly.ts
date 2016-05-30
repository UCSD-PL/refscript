
function idd<A>(x:A):A {
    return x;
}

function bar<A>(x: A, f: (x: A) => A): A {
    return f(x);
}

export function main(x: number): number {
    return bar(x, idd); // this is actually (idd @ number)
}
