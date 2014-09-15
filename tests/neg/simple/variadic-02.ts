
/*@ qualif Length(v:number, x: a): v = (len x) */

/*@ bar :: (this: #Array[#Mutable,number]) => number */
function bar () {
  this.push(1);
  return this.length;
}

var a = bar.call([1,2,3,4]);
assert(a === 4);

