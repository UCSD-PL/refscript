
/*@ interface List<M,A> */
interface List<A> {
  /*@ data :: A */
  data: A;
  /*@ next :: #List[M,A]? */
  next: List<A>;
} 

/*@ foo :: () => #List[#Immutable, {v:number| 10 < v}] */
function foo() {
  return { data: 12, next: null };
}

