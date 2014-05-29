
/*@ foo :: ({ a: { number | v = 3 } }) => number */
function foo (x):number {
  return x.a;
}
