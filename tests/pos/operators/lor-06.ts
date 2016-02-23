
/*@ foo :: (x:null, y: posint) => posint */
export function foo(x,y) {
    return x || y;      // no contextual type here -- hence using
                        // the explicit cast
}
