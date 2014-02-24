/*@ foo :: ({ [number] | (len v) = 1 } ) => { number | true } */
function foo(a : number []) : number {

  return a[0];

}
