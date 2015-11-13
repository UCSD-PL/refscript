declare module Mod {

  /*@ interface Bar<M> */
  interface Bar {
    z: number;
  }
}

class Foo {
   constructor() { }

  /*@ f : (ob: Mod.Bar<Immutable>): { number | 0 < 1 } */
  f(ob: Mod.Bar): number {
    var ans = ob.z;
    return ans
  }
}
