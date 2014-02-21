var x /*@  { f: { number | v > 0 } } */ = { f: 1 };
 
 
/*@ foo :: () => {void | true } */
function foo() {
  x.f = 2;    
}

