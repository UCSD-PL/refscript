
/*@ toNumber :: (x: string) => { number | 0 <= v }*/
function toNumber(x) {
  var n = NumberC(x);
  if (n >= 0) {
    return n;
  }
  else {
    return 0;
  }
}


/*@ foo :: (#Array[#Immutable, string]) => #Array[#Immutable,{ number | 0 < v } ] */
function foo(arr) {
  return arr.map(toNumber);
}
