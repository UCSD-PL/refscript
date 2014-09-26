

module M {
  module N {
    export class A {
        /*@ aac : number */
        public aac: number;
    }
    // TODO: exported annotations need to be globals, i.e. not SSAed
    //       give error otherwise, i.e. if annotation is missing 
    /*@        a :: { number | v > 0 } */ 
    export var a = 1; 
    

    module K { export class A { } }

    class B extends A {}
  
    export module P {

      export function foo() {}

      class B extends A {
        /*@ c : number */
        public c: number;
        /*@    a: #A[#Mutable] */
        public a: A;
      }
    }
  }
}


