

module M {

  export var a = 1;

  export var b = 2;

  b = 3;
  
  function foo() {
    var b = 1;
    b = 2; 

  }

}


module N {

   /*@ a :: number */
   var a = 2;
 
   function foo () {
     M.b = 2;
     M.b = M.b + 2;
   
   }
 

}
