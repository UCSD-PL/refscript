
/*@ qualif Poo(v:number, i:number): v = i - 1 */
/*@ qualif Poo(v:number, i:number): v = i */
/*@ qualif Poo(v:number): v < 5 */
/*@ qualif Poo(v:number): v < 4 */
/*@ qualif Poo(v:number): v <= 5 */
/*@ qualif Poo(v:number): v <= 4 */


/*@ bar :: () => { number | 4 = v } */ 
function bar(){
  var z = 1;
  for (var i = 0; i < 5; i++) {
     z = i;
  }
  return z;
}

