
/*@ foo :: () => { number | v = 10 } */
function foo () {
  arr = [2,3,5,7];
  ////Original
  //i0 = 0;
  //i1 = "1";
  //return arr[i0] + arr[i1] + arr[2] + arr["3"];
  return arr[0] + arr["1"] + arr[2] + arr["3"];
}
