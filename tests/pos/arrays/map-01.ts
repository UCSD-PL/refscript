
/*@ bar :: (#iArray[number], (number, number) => string) => {#iArray[string] | true} */ 
function bar(arr, f) {
  return arr.map(f);
}



