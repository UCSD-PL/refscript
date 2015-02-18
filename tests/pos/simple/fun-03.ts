
/*@ foo :: (x:number) => () => { undefined | true } */
function foo(x:number) {
  var f = /*@ <anonymous> () => undefined */ function() { return undefined }
  return f;
}
