
module K {
    export module L {
        export module Z {

            export class A<M extends ReadOnly> {
                public x0 = "";
                public y0 = "";
                constructor() { }
            }

        }
    }
}

module Mod {
    export class AA<M extends ReadOnly> extends K.L.Z.A<M> {
        /*@ (Immutable) k: { string | v = "K" } */
        public k = "K";

        constructor() {
            super();
        }
    }
}

module N {
    export class BB<M extends ReadOnly> extends Mod.AA<M> {
        /*@ (Immutable) l: string */
        public l: string;
        /*@ (Immutable) k: { string | v = "KK" } */
        public k: string;
        constructor() {
            super();
            this.k = "KK";
            this.l = "L";
        }
    }
}

let n = new N.BB();
