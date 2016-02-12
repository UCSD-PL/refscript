
/*@ interface MyList<M extends ReadOnly,A> */
interface MyList<M extends ReadOnly,A> {
  /*@ d : A */
  d: A;
  /*@ n : MyList<M,A> */
  n: MyList<M,A>;
}

/* a :: MyList<Mutable,number> */
let a =  { d: 1, n: { d: 2, n: null } };

/*@ main :: () => { void | 0 < 1 } */
function main() {
  assert(a.d > 1); 
}
