
/*@ qualif HasP (p: Str, x: a): hasProperty x p */


export function values<T>(m: { [k: string]: T }): void {
    for (let k in m) {
        let b = <T>(m["k"]);
    }
}
