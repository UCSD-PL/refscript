
/*@ bar :: (x: { f: number} ) => void */
function bar(a: any) {

  a.f = 1;

}

/*@ foo :: (x: [#Mutable]{ f: { number | v > 0 } }) => void */
function foo(x: any) {

  bar(x);

  assert(x.f > 0);

}
