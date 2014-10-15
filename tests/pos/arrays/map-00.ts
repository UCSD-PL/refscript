/*@ foo :: (#Array[#Immutable, number], (x:number) => string) => {#Array[#Immutable, string] | true} */ 
function foo(arr:any, f:any) {
  return arr.map(f);
}




