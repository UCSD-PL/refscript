
/*@ foo :: ((number|bool)) => top */
function foo(x) {
  return x;
}

/*@ bar :: ((number|bool)) => (top|number) */
function bar(x) {
  return x;
}

/*@ baz :: ((number|top)) => top */
function baz(x) {
  return x;
}

