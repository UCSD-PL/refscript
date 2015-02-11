module K.L {
  export module Z {
    export class A {  
      constructor () {}
    } 
  }
}

module M {  
  class B extends K.L.Z.A {   
    constructor () {
      super();
    }
  } 
}
