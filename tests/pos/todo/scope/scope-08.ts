
/*@ qualif Cmp(v: number, n: number): v = 3 */

function foo() {
  /*@ x :: number */
  var x = 2;
}

declare function baz(): number;

function bar() {

  var x = 1;
  if (true) {
    x = 3;
  }
  assert(x === 3);

  var y = 1;
  if (true) {

  }
  assert(y === 1);



}
