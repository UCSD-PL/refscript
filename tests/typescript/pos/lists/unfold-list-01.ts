
interface NumList {
  d: number;
  /*@ n :: #NumList */
  n: NumList;
}

/*@ foo :: () => number */
function foo() {
  /*@ a :: #NumList */
  var a =  { d: 1, n: { d: 2, n: null } };

  return a.n.d;

}

var aa = foo();
