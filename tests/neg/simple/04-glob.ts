
/*@ qualif CmpZ(v:int): v = 20 */

var glob = 20;

function zog(){
  glob = 3;
}

zog();

assert(glob === 20);
