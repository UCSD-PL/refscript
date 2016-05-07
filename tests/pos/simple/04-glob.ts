/*@ qualif CmpZ(v:int): v = 20 */

/*@ global */ let glob = 20;


module A {

    function zog() {
        glob = 20;
    }
    zog();

    assert(glob === 20);

}
