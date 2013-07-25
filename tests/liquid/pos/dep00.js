
/*@ foo :: (x:number, {y:number | x < y}) => number */
function foo(x, y){
  return 10;
}

/*@ baz :: ({a:number|true}, {b:number | true}) => number */
function baz(a, b){
  var r = 0;
  if (a < b){
    r = foo(a, b);
  }
  return r;
}
