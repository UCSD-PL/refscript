/*@ foo :: () => { a: { number | 987 = v } } */ 
function foo() {
  var x = { a: 1 };
  for (var i = 0; i < 5; i++) {
     x = { a: i };
  }
  return x;
}

/*@ qualif Poo(v:number, i:number): v = i - 1 */
/*@ qualif Poo(v:number, i:number): v = i */
/*@ qualif Poo(v:number): v < 5 */
/*@ qualif Poo(v:number): v < 4 */
/*@ qualif Poo(v:number): v <= 5 */
/*@ qualif Poo(v:number): v <= 4 */

