
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

/*@ foo :: (x:number) => {number|v = 3*x + 3} */
function foo(x:number):number{
    return x += incr(x++) + incr(x++);
}

/*@ bar :: (x:number) => {number|v = x + 4} */
function bar(x:number):number{
    let z = incrBy(x++, x++);
    z = incrBy(x++, x++);
    return x;
}
