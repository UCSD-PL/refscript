
module K {
  export module L {
    export module Z {

      export class A {
        public x0 = "";

        /*@ y0 : [Immutable] { string | v = "" } */
        public y0 = "";

        /*@ new () => { A<M> | true } */
        constructor() {}
      }

    }
  }
}


module Mod {
  export class AA extends K.L.Z.A { 
    /*@ k : [Immutable] { string | v = "K" } */
    public k = "K";  

    /*@ new () => { AA<M> | true } */
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

    /*@ new () => BB<M> */
    constructor() {
      super();
      this.l = "L";
      this.y0 = "000"
    }
  }
}

var n = new N.BB();
