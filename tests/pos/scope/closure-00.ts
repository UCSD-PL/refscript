

/*@ b :: { number | v >= 2 } */
var b = 2; 

/*@ foo :: () => { void | 0 < 1 } */
function foo() { 
  assert(++b >= 2); 
}

foo();

