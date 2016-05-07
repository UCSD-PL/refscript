
/*@ qualif Poo(v: int, i: int): v = i - 1 */
/*@ qualif Poo(v: int): v < 6 */


/*@ loop :: () => { number | v = 5 } */
function loop() : number{

    let y = 0;

	for(let x = 1 ;x <= 5; x ++) {
        y = x;
	}

	return y;
}
