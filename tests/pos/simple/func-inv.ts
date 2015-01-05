/*@ foo :: (f:()=>void) => {number | v > 2 } */
function foo(f) {
  if (f) return 3;
  return undefined;
}
