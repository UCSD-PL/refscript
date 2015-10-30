
/*@ foo :: ( ) =>  { number | v > 0 }*/
export function foo(): number {
    let hehehehehe = 32;

    /*@ a :: { number | v > 0 } */
    let a = 1;
    a = a + 123;

    function bar(): number {
        a = a + 1;
        return a;
    }

    return bar();
}

foo();

function foo1(): string {

    /*@ a1 :: string */
    let a1 = "1";
    a1 = a1 + "2";

    function bar1(): string {
        a1 = a1 + "3";
        return a1;
    }

    return bar1();
}

foo1();
