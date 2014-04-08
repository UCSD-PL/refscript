/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */
/*@ qualif Pos(v:number)                        : 0 <= v       */

/*@ sumLoop :: (acc: number, i: number) => number  */
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
	var n = pos();
	var m = sumLoop(0, n);
	assert(m == n);
}
