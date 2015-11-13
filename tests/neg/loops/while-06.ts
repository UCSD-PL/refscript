/*@ foo :: () => { number | 0 < 1 } */
function foo() {
  var i = 0; 
  var z : any = 10;

  while (i < 5) {
     i = i + 1; 
     z = "dog"; // whoops, should be same as outside.
  }
  
  return z;
}
