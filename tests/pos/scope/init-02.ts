
/*@ foo :: (cnd: boolean) => { number | v > 0 } */

function foo(cnd: boolean): number {
 
  if (cnd) {
    /*@ r :: number */
    var r = 1;
  }
  else {
    /*@ r :: number */
    var r = 2;
  }

  return r

}

