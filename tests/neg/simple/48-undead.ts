
/*@ noop :: (u: top) => void */
function noop(u) { }

/*@ foo :: (u : boolean) => void */
export function foo(u) {
    noop(u);
    assert(false);
}
