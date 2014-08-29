
/*@ main :: (x: number) => { v: number |v > x} */
function main(x: number): number {

	var rand = 10;
	if (rand > 5) {
		rand = rand + 1;
	}

	function plus(a: number): number { 
		return a + x 
	};
	
	var z = plus(12);

	return z;
}

