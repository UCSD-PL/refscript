
/*@ inc :: (xxx: number) => { number | v = xxx + 1 } */
function inc(xxx: number): number {
  return xxx + 1;
}

/*@ main :: () => {number | v = 0} */
function main(): number{ 
  var g = 1;

  assert(inc(g) === inc(g));
  
  return 0;

}

