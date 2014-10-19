
/*@ foo :: (n: number) => { number | true } + undefined */

function foo(n: number): number {
 
  /*@ r :: number */
  var r;
  
  while (n < 10) {

    r = 1;

    n++;

  }

  return r;

}

