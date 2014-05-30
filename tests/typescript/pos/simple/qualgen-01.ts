
/*@ interface List<A> */
interface List<A> {
  /*@ data :: A */
  data: A;
  /*@ next :: #List[A]? */
  next: List<A>;
} 

/*@ foo :: () => #List[{v:number| 10 < v}] */
function foo() {
  return { data: 12, next: null };
}

