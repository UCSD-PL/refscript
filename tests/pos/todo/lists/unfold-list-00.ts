
/*@ interface MyList<M extends ReadOnly,A> */
interface MyList<M extends ReadOnly,A> {
  /*@ d : A */
  d: A;
  /*@ n : MyList<M,A> */
  n: MyList<M,A>;
}

/* a :: MyList<Mutable,number> */
let a =  { d: 1, n: { d: 2, n: null } };

let b = a;


assert(a.d > 0); 
