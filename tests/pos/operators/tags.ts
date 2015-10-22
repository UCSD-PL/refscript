
/*@ moo :: (number) => {v:string | v = "number"} */
export function moo(x: number): string {
    let z: string = typeof (x);
    return z;
}
