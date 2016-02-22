
export function foo() {
    let a = 0;

    for (let j = 1; j <= 3; j++) {
        for (let i = 1; i <= 3; i++) {
            ++a;
        }
    }
    ++a;
}
