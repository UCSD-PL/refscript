
module M {
    export let a = 1;
    export let b = 2;
    b = 3;
    function foo() {
        let b = 1;
        b = 2;
    }
}

module N {
    /*@ a :: number */
    let a = 2;
    function foo() {
        M.b = 2;
        M.b = M.b + 2;
    }
}
