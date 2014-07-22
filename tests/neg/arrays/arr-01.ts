/*@ foo :: ({ #Array[#Immutable,number] | (len v) < 10 } ) => number */
function foo(a) {

  return a[9];

}
