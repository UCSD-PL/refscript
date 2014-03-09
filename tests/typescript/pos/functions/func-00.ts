
/*@ foo :: forall A B . (x: A, y: B) => A */
function foo<A,B>(x:A, y:B): A {

  
  /*@ bar :: forall C . (x: C) => C */
  function bar<C>(x:C): C {
    return x;
  }

  return bar(x);

}

class A {}


/*@ a :: #A */
var a = foo(1,2);

/*@ b :: string */
var b = foo(1,"");


