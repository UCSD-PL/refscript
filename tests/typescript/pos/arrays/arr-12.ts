

/*@ f1 :: ( #Array[#Immutable,number], #Array[#Mutable, number]) => { #Array[#Immutable, number] | true } */
function f1(a : number[], e : number[]) : number[] {
  return a.concat(e);
}

/*@ f2 :: ( #Array[#Immutable,number], #Array[#Mutable, number]) => { #Array[#Immutable, number] | true } */
function f2(a : number[], e : number[]) : number[] {
  return a.concat(e);
}

/*@ f3 :: ( #Array[#Immutable,number], #Array[#Mutable, number]) => { #Array[#Immutable, number] | true } */
function f3(a : number[], e : number[]) : number[] {
  return a.concat(e);
}
