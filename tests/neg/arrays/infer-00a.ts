let garr : number[] = [1,2,3,4];

/*@ foo :: () => void */
function foo() : void {
  garr[2] = 10;
}

/*@ bar :: ({number | (0 <= v && v <= 4)}) => {number | v > 0} */
function bar(n : number) : number{
  let z : number = garr[n];
  return z;
}
