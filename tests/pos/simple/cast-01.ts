
/*@ foo :: () => { number | v > 0 } + undefined */ 
declare function foo(): any;

/*@ bar :: () => { number | true } */
function bar(): number {

  var x = foo();

  if (x) {

    return 1 + <number>x;

  }

  return 0;

}

