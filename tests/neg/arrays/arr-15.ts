

/*@ revInc :: forall M . (#Array[#Mutable,number]) => { #Array[#Immutable,number] | true } */
function revInc(a: number[]) {

  a.reverse();

  for (var i = 0; i < a.length; i++) {
     a[i] = a[i] + 1; 
  }
  
  return a;

}
