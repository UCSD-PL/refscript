
/*@ resistance :: number */
let resistance = 1000;

/*@ makeNum ::    (x: number) => number */
/*@ makeNum :: <A>(x: A)      => number */
function makeNum(x: any) {
    return 1;
}


export function doCalculateResistance() {
    resistance = makeNum("1");
}
