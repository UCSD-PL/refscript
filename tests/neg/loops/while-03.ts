/*@ qualif Ineq2(v : number ): (v < 5) */

/*@ foo :: () => { number | v = 10 } */ 

function foo() {

  var x = 1;
  var i = 0; 

  while (i < 5) {
    // What is the invariant here ?
    // is x == to which i (the invariant?)
     x = i;
     i = i + 1; 
  }

  return x;

}
