
/*@ foo ::  (number) => top */
function foo(x) {
  return nil();
}


/*@ bar ::  (number) => (top|number) */
function bar(x) {
  return nil();
}

/*@ baz ::  (number) => list [top] */
function baz(x) {
  return nil();
}

/*@ bak ::  (number) => list[number]  */
function bak(x) {
  var r = 1;
  return nil();
}
