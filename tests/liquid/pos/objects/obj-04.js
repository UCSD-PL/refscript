
/*@ foo :: ({ a: { number | v = 3 }, *: boolean }) => number */
function foo (x) {
  return x.a;
}
