module K {

    export function foo(): void { }

    export module L {

      export function baz(): void { }
    
      export module M { 

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
