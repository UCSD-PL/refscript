
/*@ foo :: (cnd: boolean) => { number | v > 0 } */

function foo(cnd: boolean): number {
 
  /*@ r :: number */
  var r;
  
  if (cnd) {

    r = 1;

  }
  else {

    if (r > 5) {
      
      r = 10;
    
    }
    else {

      r = -2

    }

  }

  return r

}

