
/*@ foo :: ( ) =>  { number | v > 0 }*/
function foo( ) : number  {

  /*@ a :: { number | v > 0 } */
  var a = 1;
  a = a + 123;

   function bar( ) : number  {
     a = a + 1;
     return a;
   }
 
   return bar();
}

foo();

function foo1( ) : string  {

  /*@ a1 :: string */
  var a1 = "1";
  a1 = a1 + "2";

  function bar1( ) : string  {
    a1 = a1 + "3";
    return a1;
  }

  return bar1();
}

foo1();
