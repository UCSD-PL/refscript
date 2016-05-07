
// Use uniqueness to capture effectful operations

let a = new Array();

for (let i = 0; i < 10; i ++) {
    a.push(i);
}

assert(a.length === 10);
