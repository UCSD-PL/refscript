/*@ incr3 :: (x:number) => {number | v = x + 3} */
function incr3(x:number):number{
  ++x;
  ++x;
  x++;
  ++x;
  return x;
}
