
/*@ foo :: (cnd: boolean) => { number | true } + undefined */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  if (cnd) {

    r = 1;

  }
  return r;

}

