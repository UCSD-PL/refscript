/*@ foo :: () => { void | true } */
function foo() {
  if ( true && true ) {
    
    assert(false);
  
  }
}
