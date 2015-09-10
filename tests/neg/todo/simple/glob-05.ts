var glob = 20;

/*@ zog :: () => void */
function zog(){
  glob = 3;
}

zog();

assert(glob === 20);
