
/*@ foo2 :: () => () => { undefined | 0 < 1 } */
function foo2() {
  assert(false);
  return function() { return undefined }
}
