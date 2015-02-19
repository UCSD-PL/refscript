var garr /*@ readonly */ : number[] = [1,2,3,4];

/*@ foo :: () => void */
function foo() : void {
  garr[2] = 10;
}

/*@ bar :: ({number | (0 <= v && v <= 3)}) => {number | v > 0} */
function bar(n : number) : number{
  var z : number = garr[n];
  return z;
}
