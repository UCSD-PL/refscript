/* option "--real" */

/*@ foo :: () => {number | v = 0} */
function foo():number{

  var a = 10;

  var b = 20;

  assert(a*a + b*b > 499);
  
  return 0;

}
