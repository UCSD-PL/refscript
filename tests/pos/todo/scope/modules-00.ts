module K {

    export function foo(): void { }

    class InK { }

    export module L {

      export function baz(): void { }
        
      class InKL { }
    
      export module M { 

        class InKLM { }

          export function bar(): void {
            foo();
            K.foo();

            L.baz();
            K.L.baz();
          } 
      }
    }
}


class InK { }


module K1 {

    export function foo(): void { }

    class InK { }

    export module L {

      export function baz(): void { }
        
      class InKL extends InK { }
    
      export module M { 

        class InKLM { }

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
