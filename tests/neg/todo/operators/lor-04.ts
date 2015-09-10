
/*@ foo :: (x:null, y: { number | v > 1}) => { number | v > 2 } */
function foo(x,y) {

    var r = <number> (x || y);      // no contextual type here -- hence using
                                    // the explicit cast

    return r; 
}

