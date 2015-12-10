//adapted from transducers

/*@ foo :: () => { number | v = 4 } */
export function foo() {
  let bar = true ? 3 : null;
  return bar + 1;
}
