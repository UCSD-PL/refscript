
/*@ foo :: ( ) =>  posint */
export function foo(): number {

    /*@ a :: posint */
    let a = 1;
    a = a + 123;

    let bar: () => number = function(): number {
        a = a + 1;
        return a;
    }

    return bar();
}

foo();
