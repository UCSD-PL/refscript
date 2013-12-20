
/*@ foo :: () => { a: { number | 4 = v } } */ 

function foo() {

  var x = { a: 1 };

  for (var i = 0; i < 5; i ++) {
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


/*@ bar :: () => {number | v = 4 } */ 
function bar() {
  var z = 0;
  var i = 1;
  // for (var i = 1; i < 5; i++) {
  //    z = i;
  // }
  while (i < 5){
    z = i;
    i = i + 1;
  }

  return z;
}


