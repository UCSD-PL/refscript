

var a = 1;

/*@ foo :: () => { void | true } */
function foo() { 
  assert(a === 1); 
}

a++; 

foo();

