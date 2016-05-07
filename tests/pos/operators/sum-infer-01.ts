
// Ha ha. Why is this safe? :)

function sumLoop(acc: number, i: number) {
    let r: number = acc;

    if (0 < i) {
        r = sumLoop(acc + 1, i);
    }

    return r;
}

export function main() {
    let n: number = _pos();
    let m: number = sumLoop(0, n);
    assert(m === n);
}
