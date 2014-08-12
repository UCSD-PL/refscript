
/*@ incr :: (x:number) => {number|v = x + 1} */
function incr(x:number):number{
  return x++;
}

/*@ incrBy :: (x:number, k:number) => {number|v = x + k} */
function incrBy(x:number, k:number):number{
  return x+=k;
}

/*@ foo :: (x:number) => {number|v = 3*x + 7} */
function foo(x:number):number{
    return x += incr(x++) + incr(x++);
}