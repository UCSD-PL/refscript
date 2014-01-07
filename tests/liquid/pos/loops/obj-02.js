// related: tests/liquid/pos/loops/for-05.js 

/*@ qualif Poo(v:number): v = -1 */
/*@ qualif Poo(v:number, i:number): v = i - 1 */
/*@ qualif Poo(v:number): v < 5 */


/*@ foo :: () => { a: { number | 4 = v } } */ 
function foo() {
  var x = { a: (-1) }; //need silly singleton v = -1 qualifier as we generate a template for this x. Ugh.

  for (var i = 0; i < 5; i++) {
     x = { a: i };
  }

  return x;
}

