
/*@ foo :: (x:number) => #Array[#Immutable,{number|v=x}]  */
function foo(x) {
  return [x];
}

/*@ bar :: (cat:number) => #Array[#Immutable,{number|v>cat}]  */
function bar(y) {
  return foo(y);
}

bar(10);
