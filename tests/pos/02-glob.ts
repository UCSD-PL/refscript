/*@ [global] glob :: posint */
var glob = 12;

export function bar() {
    glob = 7;
    return;
}
