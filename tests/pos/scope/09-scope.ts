
module K {
    export module L {
        export class AAAA<M extends ReadOnly> { constructor() { } }

        export module M {
            export class B<M extends ReadOnly> extends AAAA<M> { constructor() { super(); } }
        }
    }

    class C<M extends ReadOnly> { constructor() { } }

    module N {
        class D<M extends ReadOnly> { constructor() { } }

        export module L {
            export class E<M extends ReadOnly> extends C<M> { constructor() { super(); } }
            export module M {
                export class B<M extends ReadOnly> extends K.L.AAAA<M> { constructor() { super(); } }
                export class C<M extends ReadOnly> extends L.E<M> { constructor() { super(); } }
                class F<M extends ReadOnly> extends K.L.M.B<M> { constructor() { super(); } }
                class G<M extends ReadOnly> extends L.M.B<M> { constructor() { super(); } }
            }
        }
    }
}
