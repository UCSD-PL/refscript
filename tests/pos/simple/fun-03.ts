
/*@ foo :: (x:number) => () => { undefined | 0 < 1 } */
function foo(x:number) {
  var f = function() /*@ <anonymous> () => undefined */  { return undefined }
  return f;
}

