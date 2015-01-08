/*@ foo :: (f:()=>void) => {number | v > 4 } */
function foo(f) {
  if (f) return 3;
  return undefined;
}
