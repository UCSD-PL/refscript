
/*@ readonly x_25 :: { @Assignable f: posint } */
let x_25 = { f: 1 };

export function foo(): void {
    x_25.f = 2;
}
