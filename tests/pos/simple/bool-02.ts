/*@ foo :: () => { void | 0 < 1 } */
function foo() {
  if ( true && false ) {
    
    assert(false);
  
  }
}
