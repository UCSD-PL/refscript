
/*@ foo :: ( ) =>  { number | v = 123 }*/
export function foo(): number {
    /*@ a :: number */
    let a = 1;
    a = a + 1;
    /*@ bar :: ( ) => number */
    function bar(): number {
        a = a + 1;
        return a;
    }
    return bar();
}

foo();
