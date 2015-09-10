//adapted from transducers

/*@ foo :: () => { number | v = 4 } */
function foo() {
  var bar = true ? 3 : null;
  return bar + 1;
}
