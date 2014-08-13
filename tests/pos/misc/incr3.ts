/*@ sincr :: (x:number) => {number|v = x + 1} */
function sincr(x:number):number{
  ++x;
  return x;
}

/*@ pincr :: (x:number) => {number|v = x + 1} */
function pincr(x:number):number{
  x++;
  return x;
}