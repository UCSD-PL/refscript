
/*@ main :: (x:number) => { v:number |v > x} */
function main(x:number):number{

	var rand:number = 10;
	if (rand > 5){
		rand = rand + 1;
	}

	/*@ plus :: (number) => number */
	function plus(a:number):number{ 
		return a + x 
	};
	
	var z :number= plus(12);

	return z;
}

