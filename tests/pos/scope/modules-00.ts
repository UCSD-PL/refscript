module K {

    export function foo(): void { }

    class InK { 
        constructor() {}
    }

    export module L {

      export function baz(): void { }
        
      class InKL { 
          constructor() {}
      }
    
      export module M { 

        class InKLM { 
            constructor() {}
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


class InK { 
    constructor() {}
}


module K1 {

    export function foo(): void { }

    class InK { 
        constructor() {}
    }

    export module L {

      export function baz(): void { }
        
      class InKL extends InK { 
          constructor() {
              super();
          }
      }
    
      export module M { 

        class InKLM {
            constructor() {}
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
