module K {
    export function foo(): void { }

    class InK<MMM extends ReadOnly> {
        constructor() { }
    }

    export module L {
        export function baz(): void { }

        class InKL<M extends ReadOnly> {
            constructor() {}
        }

        export module M {
            class InKLM<M extends ReadOnly> {
                constructor() { }
            }

            export function bar(): void {
                foo();
                K.foo();
                L.baz();
                K.L.baz();
            }
        }
    }
}

class InK<M extends ReadOnly> {
    constructor() { }
}

module K1 {
    export function foo(): void { }

    class InK<M extends ReadOnly> {
        constructor() { }
    }

    export module L {
        export function baz(): void { }

        class InKL<M extends ReadOnly> extends InK<M> {
            constructor() { super(); }
        }

        export module M {
            class InKLM<M extends ReadOnly> {
                constructor() { }
            }

            export function bar(): void {
                foo();
                K.foo();
                L.baz();
                K.L.baz();
            }
        }
    }
}

K.foo();
K.L.M.bar();
