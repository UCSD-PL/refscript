/*@ mycrash :: forall A. () => {v:A | true} */ 
function mycrash<A>(): A {
    var z = mycrash();
    return ;
}

/*@ init :: forall T. (number) => {v:T | true} */
function init<T>(n:number) : T {
    var x:T = mycrash();
    return x;
}