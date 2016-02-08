
/*@ foo :: (x:number + undefined, d:number) => {v:number | 0 < 1} */
function foo(x:number, d:number):number {
  var res = d;
  if (x) res = <number>x;
  return res;
}

