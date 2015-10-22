//adapted from transducers

/*@ foo :: () => { number | v = 5 } */
function foo() {
  let bar = true ? 3 : null;
  return bar + 1;
}
