//adapted from transducers

/*@ foo :: () => {string | true} */
function foo() {
  var bar = false ? 3 : null;
  return bar;
}
