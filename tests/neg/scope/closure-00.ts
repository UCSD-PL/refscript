

var a = 1;

/*@ foo :: () => { void | 0 < 1 } */
function foo() { 
  assert(a === 1); 
}

a++; 

foo();

