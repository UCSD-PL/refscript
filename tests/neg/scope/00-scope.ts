
let a = 1;

export function foo(): void {
    assert(a === 1);
}

a++;

foo();
