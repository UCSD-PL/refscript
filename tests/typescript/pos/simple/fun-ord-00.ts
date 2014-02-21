/*@ foo :: (x:number) => {number | v = x + 2} */
function foo(x){
  var a = zogbert(x);
  var b = zogbert(a);
  return b;
}

/*@ zogbert :: (x:number) => {number | v = x + 1} */
function zogbert(x){
  return x + 1;
}
