
/*@ foo2 :: () => () => { undefined | true } */
function foo2() {
  assert(false);
  return function() { return undefined }
}
