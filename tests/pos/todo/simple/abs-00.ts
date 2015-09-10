/*@ ab :: (number) => {res: number | res >= 0} */
function ab(x:number):number{
  var r = x;
  if (x > 0) {
    r = x;
  } else {
  r = (0 - x);
  }
  return r;
}

var a = ab(1)

/*@ minus :: (x:number, y:number) => {v:number | v = x - y} */
function minus(x, y) { 
  return x - y; 
}