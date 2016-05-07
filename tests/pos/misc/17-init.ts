
// crash is from the prelude.ts
// crash :: forall A. () => A
// declare function crash<A>(): A;


export function init<T>(n:number):T{
    let x:T = crash();
    return x;
}
