module K {

    export module L {
      
        export class A { 
            constructor() {}
        }
    
        export module M { 

            export class B extends A { 
                constructor() {
                    super();
                }
            }

      }
    }
}


class C { constructor() {} }


module N {

    class D { constructor() {} }

    export module L {

        export class E extends C {  
            constructor() {
                super();
            }
        }
     
        export module M { 

            export class B extends K.L.A { 
                constructor() { super(); }
            }
            
            export class C extends L.E { 
                constructor() { super(); }            
            }

            class F extends K.L.M.B { 
                constructor() { super(); }
            }

            class G extends L.M.B { 
                constructor() { super(); }            
            }

        }
    }
}

