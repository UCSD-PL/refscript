


/*@ foo :: (x: undefined + { number | v > 1}, y: { number | v > 2}) => { number | v > 0 } */
function foo(x,y) {
  
    var r = x || y;

    return r; 

}

