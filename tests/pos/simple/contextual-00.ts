

/*@ a :: forall T . (a: #Array[#Immutable, T], 
                     indexes: #Array[#Immutable, {v: number | (0 <= v && v < (len a)) }]) 
                => { #Array[#Immutable, T] | (len v) = (len indexes) } */
var a = function<T>(array: T[], indexes:number[]) : T[] {
  var i = indexes.length, permutes = new Array<T>(i);
  while (i > 0) {
    i--;
    permutes[i] = array[indexes[i]];
  }
  i--;
  return permutes;
};

