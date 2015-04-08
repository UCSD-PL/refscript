
/*@ main :: (xx: number) => { v: number |v > xx} */
function main(xx: number): number {
    
  /*@ readonly x :: # */
  var x = xx;

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

