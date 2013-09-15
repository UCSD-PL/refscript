/*@ qualif CmpTwo(v:number)    : 2 <= v                      */
/*@ qualif CmpFive(v:number)    : v <= 5                      */

/*@ foo :: ( { v: number| v > 0} ) => { number | ((2 <= v) && (v <= 5)) } */
function foo (n) {

  var a = [];  

  if (n < 5) a[0] = 2;
  else       a[0] = 5; 

  return a[0];

}
