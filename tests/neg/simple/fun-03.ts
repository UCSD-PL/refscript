/*@ foo1 :: (x:number) => () => { undefined | true } */
function foo1(x:number) {
  assert(false);
  return function() { return undefined }
}
