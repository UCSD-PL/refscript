
/*@ foo :: (x: number) => [{ number | v = x }]  */
function foo(x : number) : number[] {
  return [x];
}

/*@ bar :: (y: number) => [{ number | v = y }]  */
function bar(y : number) : number[] {
  return foo(y);
}

