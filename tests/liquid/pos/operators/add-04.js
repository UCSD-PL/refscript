/*@ bog :: (x:{number | true} + {string | false}) => {number | v=x} + {string | false} */
function bog(x){
    return x;
}

/*@ gloop :: (number) => number */
function gloop(y){
    var z = bog(y);
    return z;
}
