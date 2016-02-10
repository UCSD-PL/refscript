
/*@ nincr :: (x:number) => {number|v = x} */
function nincr(x:number):number{
  return x++;
}

/*@ incr :: (x:number) => {number|v = x + 1} */
function incr(x:number):number{
  return ++x;
}

/*@ incrBy :: (x:number, k:number) => {number|v = x + k} */
function incrBy(x:number, k:number):number{
  let z = x + k;
  return z;
}

/*@ bar :: (x:number) => {number|v = x + 4} */
function bar(x:number):number{
  let z = incrBy(x++, x++);
  z = incrBy(x++, x++);
  return x;
}

