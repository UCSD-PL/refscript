
/*@ foo ::  (int) => top */
function foo(x) {
  return nil();
}


/*@ bar ::  (int) => (top|int) */
function bar(x) {
  return nil();
}

/*@ baz ::  (int) => list [top] */
function baz(x) {
  return nil();
}

/*@ bak ::  (int) => list[int]  */
function bak(x) {
  var r = 1;
  return nil();
}
