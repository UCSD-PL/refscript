


/*@ foo :: (x:null, y: { number | v > 1 } ) => { number | v > 11 } */
function foo(x,y) {
    return x || y;      // works due to contextual type "nunber"
}

