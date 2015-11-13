
/*@ alias iArray[T]    = #Array[#Immutable, T]        */

// TODO: *******MUST****** report error locations for malformed type-aliases!!!! 

/*@ baz :: forall T. (#iArray) => {v:number | 0 < 1} */
function baz<T>(array: T[]): number {

  var i = 0, 
      n = array.length - 1, 
      p0, p1 = array[0], 
      pairs = new Array(n < 0 ? 0 : n);
 
  return i;
}