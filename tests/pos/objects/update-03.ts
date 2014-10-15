
/*@ bar :: (x: [#Mutable]{ f: [#Mutable] number} ) => void */
function bar(a: any) {

  a.f = 1;

}

/*@ foo :: (x: [#Mutable]{ f: [#Mutable] { number | v > 0 } }) => void */
function foo(x: any) {

  bar(x);

  assert(x.f > 0);

}
