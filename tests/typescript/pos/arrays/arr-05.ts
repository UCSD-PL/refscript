
/*@ foo :: (x:number) => [{number | v=x}]  */
function foo(x : number) : number[] {
  return [x];
}

/*@ bar :: (cat:number) => [{number | v=cat}]  */
function bar(y : number) : number[] {
  return foo(y);
}

