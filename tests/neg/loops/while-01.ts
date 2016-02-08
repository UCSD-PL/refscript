


/*@ loop :: () => { number | v = 800 } */
function loop() {
  let x = 1;
  while (x <= 5) {
    x = x + 1;
  }
  return x;
}
