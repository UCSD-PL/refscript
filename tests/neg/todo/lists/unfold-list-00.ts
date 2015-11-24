
/*@ interface MyList<A> */
interface MyList<A> {
  /*@ d :: A */
  d: A;
  /*@ n :: #MyList[A] */
  n: MyList<A>;
}

/*@ a :: #MyList[number] */
var a =  { d: 1, n: { d: 2, n: null } };

/*@ main :: () => { void | 0 < 1 } */
function main() {
  assert(a.d > 1); 
}
