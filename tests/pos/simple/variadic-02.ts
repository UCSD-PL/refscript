/*@ bar :: forall M T. (this: { Array<M,T> | (len v) = 4 }) => { number | v = (len this) } */
function bar () {

  return this.length;
}

var a = bar.call([1,2,3,4]);
assert(a === 4);




///*@ bar :: forall M T. (x: #Array[M,T]) => number */
//function bar(x) {

//  var a = x.length;

  
//  return a;
//}

//var a = [1,2,3,4];

//var l = bar(a);

//assert(l === 4);
