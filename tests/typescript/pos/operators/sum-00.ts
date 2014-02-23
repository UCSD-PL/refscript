/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

/*@ sumLoop :: ({acc:number | true}, {i:number| 0 <= i}) => {v:number|v = acc + i} */
function sumLoop(acc:number, i:number):number{
	var r :number= 0;
	if (0 < i){
		r = sumLoop(acc + 1, i - 1);
	} else {
		r = acc;
	}
	return r;
}

/*@ main :: () => void */
function main():void{
	var n :number= pos();
	var m :number= sumLoop(0, n);
	assert(m == n);
}
