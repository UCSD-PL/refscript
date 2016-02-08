
/*@ foo :: (x:number + undefined, d:number) => {v:number | 0 < 1} */
function foo(x:number, d:number):number {
  var res = x ? <number>x : d;
  return res;
}

