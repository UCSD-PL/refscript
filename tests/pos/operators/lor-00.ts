


/*@ foo :: (x:null, y:number) => { number | 0 < 1 } */
function foo(x,y) {
    return x || y;      // works due to contextual type "nunber"
}

