/*@ [global] glob :: { number | v > 0 } */
var glob = 12;

export function bar() {
    glob = 7;
    return;
}
