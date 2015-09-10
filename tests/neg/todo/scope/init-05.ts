
/*@ foo :: (cnd: boolean) => { number | true } */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  if (cnd) {

    r = 1;

  }
  return r;

}

