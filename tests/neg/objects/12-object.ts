
/*@ foo :: ((ReadOnly) { x: number }) => void */
export function foo(o: any) {
    o.x = "aaa";
}
