
module K {
  export module L {
    export module Z {

      export class A {
        public x0 = "";
        public y0 = "";
        constructor() {}
      }

    }
  }
}


module Mod {
  export class AA extends K.L.Z.A { 
    /*@ k : [Immutable] { string | v = "K" } */
    public k = "K";  

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
    constructor() {
      super();
      this.k = "KK";
      this.l = "L";
    }
  }
}

var n = new N.BB();
