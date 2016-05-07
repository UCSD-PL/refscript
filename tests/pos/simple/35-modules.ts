module KKKK {

    export function foo(): void { }

    export module L {

        export function baz(): void { }

        export module M {
            export function bar(): void {
                foo();
                KKKK.foo();
                L.baz();
                KKKK.L.baz();
            }
        }
    }
}

KKKK.foo();

KKKK.L.M.bar();
