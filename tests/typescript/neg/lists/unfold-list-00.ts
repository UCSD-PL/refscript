
/*@ interface MyList<A> */
interface MyList<A> {
  /*@ d :: A */
  d: A;
  /*@ n :: #MyList[A] */
  n: MyList<A>;
}

/*@ a :: #MyList[number] */
var a =  { d: 1, n: { d: 2, n: null } };

var b = a;


assert(a.d > 1); 
