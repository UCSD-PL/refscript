
let a = 1;



module A {


    export function foo(): void {
        assert(a === 1);
    }

    a++;

    foo();

}
