//adapted from transducers

/*@ foo :: () => {string | 0 < 1} */
function foo() {
  let bar = false ? 3 : null;
  return bar;
}
