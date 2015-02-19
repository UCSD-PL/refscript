
/*@ foo :: (x:number) => () => { undefined | true } */
function foo(x:number) {
  var f = function() /*@ <anonymous> () => undefined */  { return undefined }
  return f;
}

