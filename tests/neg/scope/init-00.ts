
/*@ foo :: (cnd: boolean) => { number | v > 0 } */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  r = -1;

  return r

}

