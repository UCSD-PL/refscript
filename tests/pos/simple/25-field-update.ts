
/*@ y :: { } */
declare let y;

/*@ x :: { f: [Mutable] posint } */
let x = { f: 1 };

/*@ foo :: () => void */
function foo(): void {
    x.f = 2;
}

export function main(): void {
    foo();
}
