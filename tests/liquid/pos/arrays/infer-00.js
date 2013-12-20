var garr = [1,2,3,4];

/*@ foo :: () => void */
function foo(){
  garr[2] = 10;
}

/*@ bar :: ({number | (0 <= v && v <= 3)}) => {number | v > 0} */
function bar(n){
  var z = garr[n];
  return z;
}
