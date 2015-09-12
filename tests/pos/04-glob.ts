/*@ qualif CmpZ(v:int): v = 20 */

/*@ glob :: # */
var glob = 20;

function zog() {
    glob = 20;
}
zog();

assert(glob === 20);
