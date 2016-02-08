/*@ loop :: () => { number | v = 1 } */
function loop(){
  let x = 1;
  while (x === 1) {
    x = 2;
  }
  return x;
}
