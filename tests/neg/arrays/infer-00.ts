var garr = [0,1,2,3,4];

/*@ foo :: () => void */
function foo(){
  garr[2] = 0;
}

/*@ bar :: ({number | (0 <= v && v <= 4)}) => {number | v > 0} */
function bar(n){
  var z = garr[n];
  return z;
}
