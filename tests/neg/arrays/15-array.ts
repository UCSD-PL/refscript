
/*@ where :: ( ) => { IArray<number> | len v = 5 } */
function where( ) {
	let result = new Array<number>(5);
	while (result.push(1)) { }
	return result;
}
