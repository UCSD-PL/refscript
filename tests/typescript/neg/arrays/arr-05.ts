
/*@ foo :: (x:number) => [{number | v=x}]  */
function foo(x) {
  return [x];
}

/*@ bar :: (cat:number) => [{number | v > cat}]  */
function bar(y) {
  return foo(y);
}

bar(10);
