
// Use uniqueness to capture effectful operations


function addRand(a: Array<number>): void {
    a.push(random());
}

let a = new Array();

addRand(a);
addRand(a);

assert(a.length === 2);
