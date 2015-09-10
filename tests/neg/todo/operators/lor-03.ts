


/*@ foo :: (x: undefined + { number | v > 1}, y: { number | v > 3}) => { number | v > 2 } */
function foo(x,y) {
  
    var r = x || y;

    return r; 

}

