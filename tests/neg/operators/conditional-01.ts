//adapted from transducers

/*@ foo :: () => {string | true} */
function foo() {
  let bar = true ? 3 : null;
  return bar;
}
