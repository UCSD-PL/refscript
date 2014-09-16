
/*@ foo :: () => { number | v > -2 } + undefined */ 
declare function foo(): any;

/*@ bar :: () => { number | v > 0  } */
function bar(): number {

  var x = foo();

  if (x) {

    return 1 + <number>x;

  }

  return 1;

}

