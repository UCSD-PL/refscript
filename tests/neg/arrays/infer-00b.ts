let garr : number[] = [1,2,3,4];

/*@ foo :: () => void */
function foo() : void {
  garr[2] = 0;
}

/*@ bar :: ({number | (0 <= v && v <= 3)}) => {number | v > 0} */
function bar(n : number) : number{
  let z : number = garr[n];
  return z;
}
