
/*@ qualif CmpZ(v:int): v = 20 */

let glob = 20;

function zog(){
  glob = 3;
}

zog();

assert(glob === 20);
