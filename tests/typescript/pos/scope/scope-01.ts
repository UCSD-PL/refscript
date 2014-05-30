
/*@ g :: number */
var g = 1;

/*@ inc :: (xxx: number) => { number | v = xxx + 1 } */
function inc(xxx: number): number {
  return xxx + 1;
}

assert(inc(g) === inc(g));

