
/*@ foo :: ({ a: { number | v = 3 }, *: boolean }) => boolean */
function foo (x):boolean {
  return x.b;
}
