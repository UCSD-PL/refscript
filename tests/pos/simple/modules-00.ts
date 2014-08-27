module M {

    export var s = "hello";

    export function foo(): string {
        return s;
    }

    export module N {
    
      export function bar(): number {
        return 2;      
      } 
    }
}

M.foo();

M.N.bar();
