module M {

    /*@ s :: { string | v = "hello" } */
    export var s = "hello";

    /*@ f :: () => string */
    export function f() {
        return s;
    }
}

M.f();