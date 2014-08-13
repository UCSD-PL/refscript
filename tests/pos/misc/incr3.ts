/*@ sincr :: (x:number) => {number | v = x + 1} */
function sincr(x:number):number{
  ++x;
  return x;
}

/*@ incr3 :: (x:number) => {number | v = x + 3} */
function incr3(x:number):number{
  ++x;
  x++;
  ++x;
  return x;
}


/*@ pincr :: (x:number) => {number | v = x + 1} */
function pincr(x:number):number{
  x++;
  return x;
}