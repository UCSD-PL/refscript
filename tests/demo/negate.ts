/*@ negate :: (x: number)  => { number  | v > 0 <=> x < 0   } */
/*@ negate :: (x: boolean) => { boolean | Prop v <=> not (Prop x) } */
export function negate(x): any {
    if (typeof(x) === "number") {
        return 0 - x;
    } else {
        return !x;
    }
}

let a: number = negate(10);
let b: boolean = negate(true);
