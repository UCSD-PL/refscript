
/*@ foo :: (cnd: boolean) => { number | 0 < 1    } */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  if (cnd) {

    r = 1;

  }
  return r;

}

