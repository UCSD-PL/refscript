

class A { 
    constructor() {}
    public a: string = "" 
}

module P {

  class A { 
      constructor() {}

      public a: number = 1; 
  }

}

module N {

  class B extends A { 
          constructor() { super(); }
  }
 
  module K {
    
    class C extends B {

      constructor() { super(); }

      public foo() {
        var a = 1; 
        return a + 1;
      }
    
    }

    class D extends A { 
        constructor() {
            super();
        } 
    }

    class E extends A { 
            constructor() { super(); }
    }

  }

}

