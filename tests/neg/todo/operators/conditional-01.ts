//adapted from transducers

/*@ foo :: () => {string | true} */
function foo() {
  var bar = true ? 3 : null;
  return bar;
}
