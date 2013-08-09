
/*@ foo :: (number + boolean) => top */
function foo(x) {
  return x;
}

/*@ bar :: (number + boolean) => top + number */
function bar(x) {
  return x;
}

/*@ baz :: (number + top) => top */
function baz(x) {
  return x;
}

