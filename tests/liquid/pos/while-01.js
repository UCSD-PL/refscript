

/*@ loop :: () => { number | v = 6 } */
function loop(){  
  var x = 1;
  while (x <= 5) {
    x = x + 1; 
  }
  return x;
}

/*@ main :: () => void */
function main(){
  loop();
}
