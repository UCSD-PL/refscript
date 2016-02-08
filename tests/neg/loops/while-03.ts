/*@ qualif Ineq2(v : number ): (v < 5) */

/*@ foo :: () => { number | v = 10 } */

function foo() {

  let x = 1;
  let i = 0;

  while (i < 5) {
    // What is the invariant here ?
    // is x == to which i (the invariant?)
     x = i;
     i = i + 1;
  }

  return x;

}
