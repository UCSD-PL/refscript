

/*@ b :: { number | v >= 2 } */
var b = 2; 

/*@ foo :: () => { void | true } */
function foo() { 
  assert(++b >= 2); 
}

foo();

