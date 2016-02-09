
/* option "--real" */

/*@ incr :: (x:number) => {number|v = x + 1} */
function incr(x:number):number{
    return ++x;
}

/*@ incrBy :: (x:number, k:number) => {number|v = x + k} */
function incrBy(x:number, k:number):number{
  return x += k;
}

/*@ efoo :: (x:number) => {number|v = x + 7} */
function efoo(x:number):number{
    return x += 7;
}

/*@ foo :: (x:number) => {number|v = x + x + x + 3} */
function foo(x:number):number{
/* x1 = x0 + 1
   x2 = x0 + 2
   t1 = x0 + 1
   t2 = x1 + 1 = x0 + 2
  t3 = x0 + 2 + x0 + 1 + x0 +2
  = 3*x + 5
  */

  return x += incr(x++) + incr(x++);
}

/*@ bar :: (x:number) => {number|v = x + 4} */
function bar(x:number):number{
    var z = incrBy(x++, x++);
    z = incrBy(x++, x++);
    return x;
}

