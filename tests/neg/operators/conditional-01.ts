//adapted from transducers

/*@ foo :: () => {string | 0 < 1} */
function foo() {
  let bar = true ? 3 : null;
  return bar;
}
