
/*@ foo :: (x:int, {y:int | x < y}) => int */
function foo(x, y){
  return 10;
}

/*@ baz :: ({a:int|true}, {b:int | true}) => int */
function baz(a, b){
  var r = 0;
  if (a < b){
    r = foo(a, b);
  }
  return r;
}
