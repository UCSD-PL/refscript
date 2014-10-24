


/*@ foo :: (x:null, y:number) => { number | true } */
function foo(x,y) {
    return x || y;      // works due to contextual type "nunber"
}

