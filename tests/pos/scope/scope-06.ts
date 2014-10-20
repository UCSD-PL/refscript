

class A { public a: string; }

module P {

  class A { public a: number; }

}

module N {

  class B extends A { }
 
  module K {
    
    class C extends B {
    
      public foo() {
        var a = 1; 
        return a + 1;
      }
    
    }

    class D extends A { }

    class E extends A { }

  }

}

