
/*@ foo :: ( ) =>  { number | v > 0 }*/
function foo( ) : number  {

  /*@ a :: { number | v > 0 } */
  var a = 1;
  a = a + 123;

   /*@ bar :: ( ) => number */
   function bar( ) : number  {
     a = a + 1;
     return a;
   }
 
   return bar();
}

foo();

/*@ foo1 :: ( ) => { string | true } */
function foo1( ) : string  {

  /*@ a1 :: { string | true } */
  var a1 = "1";
  a1 = a1 + "2";

  /*@ bar1 :: ( ) => string */
  function bar1( ) : string  {
    a1 = a1 + "3";
    return a1;
  }

  return bar1();
}


var a: Immutable;
