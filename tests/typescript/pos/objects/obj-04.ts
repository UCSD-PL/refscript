
/*@ foo :: ({ a: { number | v = 3 }, *: boolean }) => number */
function foo (x):number {
  return x.a;
}
