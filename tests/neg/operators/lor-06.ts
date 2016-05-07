
/*@ foo :: (x:null, y: number) => posint */
export function foo(x,y) {
    return x || y;      // no contextual type here -- hence using
                        // the explicit cast
}
