

/*@ foo :: ( #Array[#Mutable,number], #Array[#Mutable, number]) => { #Array[#Immutable, number] | true } */

function foo(a : number[], e : number[]) : number[] {

  return a.concat(e);

}
