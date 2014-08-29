
/*@ foo :: (x: number) => #Array[#Immutable,{ number | v = x }]  */
function foo(x : number) : number[] {
  return [x];
}

/*@ bar :: (y: number) => #Array[#Immutable, { number | v = y }]  */
function bar(y : number) : number[] {
  return foo(y);
}

