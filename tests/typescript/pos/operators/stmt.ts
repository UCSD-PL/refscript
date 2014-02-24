
/*@ foo :: (number, number) => number */
function foo(x:number, y:number):number{
  x = x + 1;
  y = y + 1;
  assert(x == y);
  return x + y;
}
