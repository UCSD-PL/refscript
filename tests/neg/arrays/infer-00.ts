var garr : number[] = [1,2,3,4];

/*@ foo :: () => void */
function foo() : void {
  garr[2] = 0;
}

/*@ bar :: ({number | (0 <= v && v <= 4)}) => {number | v > 0} */
function bar(n : number) : number{
  var z : number = garr[n];
  return z;
}
