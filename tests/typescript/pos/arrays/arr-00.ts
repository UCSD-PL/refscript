
/*@ foo :: ( a: { #Array[#Immutable,number] | (len v) > 0 } ) => number */

function foo( a : number []) : number  {

  return a[0];
}

foo([1]);
