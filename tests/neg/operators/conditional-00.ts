//adapted from transducers

/*@ foo :: () => {string | true} */
function foo() {
  let bar = false ? 3 : null;
  return bar;
}
