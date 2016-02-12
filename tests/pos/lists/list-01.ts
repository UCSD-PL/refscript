
/*@ qualif Gt10(v:number): 10 < v */

/*@ foo :: () => List<Unique, {v:number| 10 < v}> */
function foo() {
  return { data: 12, next: null };
}

