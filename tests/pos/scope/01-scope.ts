
/*@ foo :: ( ) =>  posint */
export function foo(): number {

    /*@ a :: posint */
    let a = 1;
    a = a + 123;

    function bar(): number {
        a = a + 1;
        return a;
    }

    return bar();
}

foo();
