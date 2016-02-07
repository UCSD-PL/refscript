
module K {
  export module L {
    export module Z {

      export class A {
        public x0 = "";
        public y0 = "";

        /*@ new () => { A<M> | 0 < 1 } */
        constructor() {}
      }

    }
  }
}


module Mod {
  export class AA extends K.L.Z.A { 
    /*@ k : [Immutable] { string | v = "K" } */
    public k = "";  

    /*@ new () => { AA<M> | 0 < 1 } */
    constructor() {
      super();
    }
  }
}

module N {
  export class BB extends Mod.AA {
    /*@ l : [Immutable] string */
    public l: string;
    /*@ k : [Immutable] { string | v = "KK" } */
    public k: string;

    /*@ new () => { BB<M> | 0 < 1 } */
    constructor() {
      super();
      this.k = "KK";
      this.l = "L";
    }
  }
}

var n = new N.BB();
