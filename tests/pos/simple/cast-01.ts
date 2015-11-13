
/*@ foo :: () => { number | v > 0 } + undefined */ 
declare function foo(): any;

/*@ bar :: () => { number | 0 < 1 } */
function bar(): number {

  var x = foo();

  if (x) {

    return 1 + <number>x;

  }

  return 0;

}

