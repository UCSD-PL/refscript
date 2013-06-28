
/*@ foo :: (number, number) => number */
function foo(x, y){
  x = x + 1;
  y = y + 1;
  assert(x == y);
  return x + z;
}
