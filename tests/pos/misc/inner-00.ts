
/*@ main :: (xx: number) => { v: number |v > x} */
function main(xx: number): number {
  
var x /*@ readonly */ = xx;

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

