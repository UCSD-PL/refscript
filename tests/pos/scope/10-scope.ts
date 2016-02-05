
export function foo1(): string {

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
