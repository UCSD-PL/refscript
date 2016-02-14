
/*@ foo :: () => List<Immutable, {v:number| 10 < v}> */
function foo() {
  let obj = { data: 12, next: null };

  return obj;
}
