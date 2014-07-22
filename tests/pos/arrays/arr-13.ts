
/*@ foo :: forall A . ( #Array[#Mutable, number], number ) => { #Array[#Mutable, number] | true } */
function foo(a : number[], e : number) : number[] {
  a.push(e);
  return a;
}
