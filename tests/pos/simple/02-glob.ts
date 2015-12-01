
/*@ global glob :: posint */
let glob = 12;

function bar() {
    glob = 7;
    return;
}

export function zoo() {
    bar();
    assert(glob > 0);
}
