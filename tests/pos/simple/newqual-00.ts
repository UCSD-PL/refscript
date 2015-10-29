/* qualif Eq10(v:number): v = 10 */

function foo():number{
  return 20;
}

/*@ main :: (x:number) => {v:number | true} */
function main(x:number):number { 
  var a = foo();
  assert(a === 10);
  return a;
}
