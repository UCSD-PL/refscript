
// crash is from the prelude.ts
// crash :: forall A. () => A 
// declare function crash<A>(): A; 


/*@ init :: forall T. (number) => {v: T | true} */
function init<T>(n:number):T{
    var x:T = crash();
    return x;
}