
/*@ foo :: (number, boolean) => {number| 0 < 1} */
function foo(x, y){
  x = x + 1;
  y = y + 1;
  assert(x === y);
  return x + y;
}
