
// This works, but what is `int` (in .ts land) ?

/* qualif Eq5(v:int): v = 5 */

// This fails, because currently parsed as `num @(0)` (yikes!)

/* qualif Eq5(v:number): v = 5 */

// GOAL: get this new syntax to work

/*@ qualif Eq5(v:number) { v = 5 } */


/*@ foo :: () => number */
function foo():number{
  return 5;
}

/*@ main :: (x:number) => {v:number | v = 10} */
function main(x:number):number {
  var a = foo();
  return (a + 5);
}
