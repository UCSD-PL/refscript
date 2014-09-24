

/*@  idd :: forall T. (x:T) => T */
function idd<A>(x:A):A {
    return x;
}

/*@ bar :: forall A. (A, (x:A) => A) => A */
function bar(x:any, f:any){
    return f(x);
}

/*@ main :: () => { number | v > 5 } */
function main(){
    return bar(10, idd); // this is actually (idd @ number)
}
