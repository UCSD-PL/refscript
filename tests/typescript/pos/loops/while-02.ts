
/*@ loop :: () => { number | v != 1 } */
function loop() : number{
	var x : number= 1;
	while (x == 1) {
		x = 2;
	}
	return x;
}

