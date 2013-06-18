
/*@ foo :: ((int|bool)) => top */
function foo(x) {
  return x;
}

/*@ bar :: ((int|bool)) => (top|int) */
function bar(x) {
  return x;
}

/*@ baz :: ((int|top)) => top */
function baz(x) {
  return x;
}

