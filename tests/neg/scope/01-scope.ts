
/*@ foo :: ( ) =>  { number | v = 123 }*/
export function foo(): number {
    /*@ a :: number */
    let a = 1;
    a = a + 1;

    let bar: () => number = function() {
        a = a + 1;
        return a;
    }
    return bar();
}

foo();
