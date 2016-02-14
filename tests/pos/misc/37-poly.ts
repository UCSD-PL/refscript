

/*@  idd :: <T>(x:T) => T */
function idd<A>(x:A):A {
    return x;
}

/*@ bar :: <A>(A, (x:A) => A) => A */
function bar(x:any, f:any){
    return f(x);
}

export function main(x: number): number {
    return bar(x, idd); // this is actually (idd @ number)
}
