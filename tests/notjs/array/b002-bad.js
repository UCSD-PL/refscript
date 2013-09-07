
/*@ foo :: ( [ { v: number| v > 0} ] + [boolean] ) => { number | true } */
function foo (x) {
  return x[1];

}
