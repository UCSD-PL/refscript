

/*@  idd :: forall T. (x:T) => T */
function idd<A>(x:A):A {
    return x;
}

/*@ bar :: forall A. (A, (x:A) => A) => A */
function bar(x:any, f:any){
    return f(x);
}

/*@ main :: (x: { number | v > 5 }) => { number | v > 10 } */
function main(x){
    return bar(x, idd); // this is actually (idd @ number)
}
