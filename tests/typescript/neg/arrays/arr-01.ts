/*@ foo :: ({ [number] | (len v) < 10 } ) => { number | true } */
function foo(a) {

  return a[9];

}
