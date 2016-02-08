
// TODO: FIX SSA in loop !!!




/*@ qualif Locked(v: int): v != 0   */
/*@ qualif Unlocked(v: int): v = 0  */
/*@ qualif CmpO(v: int): v >  1 */
/*@ qualif One(v: int): v = 1 */

// Only one of the two below is needed...

/*@ qualif CondLock1(v: int, x: int): v = (if (0 < x) then 1 else 0)  */
/*@ qualif CondLock2(v: int, x: int): ((0 < x) <=> (v = 0))  */

function create(): number {
    return 0;
}

function acquire(l: number): number {
    assert(l === 0);
    return 1;
}

function release(l: number): number {
    assert(l !== 0);
    return 0;
}

function loop(n: number, l: number): number {
    let flag = random();
    if (0 < n) {
        if (0 < flag) {
            l = acquire(l);
        }
        if (0 < flag) {
            l = release(l);
        }
        loop(n - 1, l);
    }
    return l;
}

/*@ main :: (n: posint) => void */
function main(n: number): void {
    let flag = random();
    let l = create();
    assert(l === 0)
    loop(n, l);
    assert(l === 0);
}
