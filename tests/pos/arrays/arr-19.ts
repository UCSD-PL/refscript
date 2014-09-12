
/*@ bar :: (a: #Array[#Immutable, number]) => void */
function bar(a: number[]) {

  a[0] = 1;

}

/*@ foo :: (a: { #Array[#Immutable, { number | v > 0 }] | (len v ) > 0 }) => void */
function foo(a: number[]) {

  bar(a);

  assert(a[0] > 0);

}
