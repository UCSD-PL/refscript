module K {

    export module L {
      
        export class A { }
    
        export module M { 

            export class B extends A { }

      }
    }
}


class C { }


module N {

    class D { }

    export module L {

        export class E extends C { }
    
        export module M { 

            export class B extends K.L.A { }
            
            export class C extends L.E { }

            class F extends K.L.M.B { }

            class G extends L.M.B { }

        }
    }
}

