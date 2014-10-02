
/*@ bar :: (#Array[#Immutable, number], (number, number) => string) => {#Array[#Immutable, string] | true} */ 
function bar(arr, f) {
  return arr.map(f);
}



