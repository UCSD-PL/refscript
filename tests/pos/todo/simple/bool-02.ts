/*@ foo :: () => { void | true } */
function foo() {
  if ( true && false ) {
    
    assert(false);
  
  }
}
