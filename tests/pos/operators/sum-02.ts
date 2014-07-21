
/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc:number, i:number):number{
	var r :number= acc;
	if (0 < i){
		r = sumLoop(acc + 1, i - 1);
	}  
	return r;
}

/*@ main :: () => {void | true} */
function main():void{
	var n:number = pos();
	var m:number = sumLoop(0, n);
	assert(m === n);
}
