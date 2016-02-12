
/*@ local g :: number */
var g = 1;

/*@ inc :: (x: number) => { number | v = x + 1 } */
function inc(x: number): number {
  return x + 1;
}

/*

/@ main :: () => {number | v = 0} /
function main(): number{ 
  var g = 1;

  assert(inc(g) === inc(g));
  
  return 0;

}
*/

assert(inc(g) === inc(g));
