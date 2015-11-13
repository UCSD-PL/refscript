
// crash is from the prelude.ts
// crash :: forall A. () => A 
// declare function crash<A>(): A; 


/*@ init :: forall T. (number) => {v: T | 0 < 1} */
function init<T>(n:number):T{
    var x:T = crash();
    return x;
}