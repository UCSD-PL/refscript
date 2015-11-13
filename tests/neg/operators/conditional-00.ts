//adapted from transducers

/*@ foo :: () => {string | 0 < 1} */
function foo() {
  var bar = false ? 3 : null;
  return bar;
}
