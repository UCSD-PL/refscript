
/*@ foo :: (x: posint, y: { string | v = "" }, z: boolean) => posint */
export function foo(x: number, y: string, z: boolean) {
    if (x && (y || z)) {
        return 1;
    }
    return 0;
}
