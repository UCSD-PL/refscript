/*@ bog :: (xag:{number | true} + {string | false}) => {number | v=xag} + {string | false} */
function bog(x){
  return x;
}

/*@ gloop :: (number) => number */
function gloop(y:number):number{
	var z:number = bog(y);
  return z;
}


