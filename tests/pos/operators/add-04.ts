/*@ bog :: /\ (xag: number) => {number | v=xag} 
           /\ (xag: { string | false } ) => string */ 
function bog(x){
  return x;
}

/*@ gloop :: (number) => number */
function gloop(y:number):number{
	var z:number = bog(y);
  return z;
}


