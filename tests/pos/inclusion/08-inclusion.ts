/*@ qualif HasP (p: Str, x: a): hasProperty x p */
/*@ qualif EnumP (p: Str, x: a): enumProp x p */

/*@  values :: <T>(mp: (Immutable) { [k: string]: T }, key: string) =>  void */
export function values<T>(mp: { [k: string]: T }, key: string): void {
    if (key in mp) {
        let a = <T>(mp[key]);
    }
};
