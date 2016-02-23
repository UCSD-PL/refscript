
/*@ readonly x_25 :: (Mutable) { f: posint } */
let x_25 = { f: 1 };

module A {

    export function foo(): void {
        x_25.f = 0;
    }

}
