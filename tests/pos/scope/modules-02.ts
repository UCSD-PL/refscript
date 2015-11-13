declare module Mod {
  interface Bar {
    z: number;
  }
}

class Foo {
   constructor() { }            
    
  /*@ f : (ob: Mod.Bar): { number | true } */
  f(ob: Mod.Bar): number {
    var ans = ob.z;
    return ans
  }
}
