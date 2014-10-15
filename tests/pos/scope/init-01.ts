
/*@ foo :: (cnd: boolean) => { number | v > 0 } */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  if (cnd) {
    r = 1;
  }
  else {

    r = 2;
  }

  return r

}

