
export function foo1(): string {

    /*@ a1 :: string */
    let a1 = "1";
    a1 = a1 + "2";

    let bar1: () => string = function() {
        a1 = a1 + "3";
        return a1;
    }

    return bar1();
}

foo1();
