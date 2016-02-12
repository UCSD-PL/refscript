
interface NumList<M extends ReadOnly> {
  d: number;
  /*@ n : NumList<M> + null */
  n: NumList<M>;
}

/*@ foo :: () => number */
function foo() {
  /*@ a :: NumList<Unique> */
  let a =  { d: 1, n: { d: 2, n: null } };

  return a.n;

}

let aa = foo();
