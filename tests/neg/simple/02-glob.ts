
/*@ global glob :: posint */
let glob = 4;


module A {

    function bar() {
        glob = 7;
        return;
    }

    export function zoo() {
        bar();
        assert(glob > 10);
    }
}
