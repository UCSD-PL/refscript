
interface List<A> {
  data: A;
  /*@ next : #List[M,A] + null */
  next: List<A>;
} 

/*@ foo :: () => #List[#Immutable, {v:number| 10 < v}] */
function foo() {
  return { data: 12, next: null };
}

