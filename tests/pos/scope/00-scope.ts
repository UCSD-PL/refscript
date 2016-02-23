
/*@ b :: { number | v >= 2 } */
let b = 2;


module A {


    export function foo(): void {
        assert(++b >= 2);
    }

    foo();


}
