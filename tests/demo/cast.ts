
/*@ foo :: () => number + undefined */ 
declare function foo(): any;

/*@ bar :: () => { number | 0 < 1 } */
function bar(): number {

  var x = foo();

  if (x) { // Try removing this undefined check

    return 1 + <number>x;

  }

  return 0;

}
