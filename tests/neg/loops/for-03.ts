
/*@ foo :: () => { number | v = 10 } */

function foo() {

  let x = 1;

  for (let i = 0; i < 5; i ++) {
     x = i;
  }

  return x;

}
