declare function foo(): number;


function bar() {
  var x = 1;
  if (foo() > 0) {
    x = 3;
  }
  assert(x === 1);
}
