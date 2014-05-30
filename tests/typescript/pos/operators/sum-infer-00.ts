
/*@ qualif Plus(v:number, x:number, y:number)   : v = x + y    */

// Ha ha. Why is this safe? :)

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc:number, i:number):number{
	var r :number= 0;
	if (0 < i){
		r = sumLoop(acc + 1, i - 1);
	} else {
		r = acc;
	}
	return r;
}

/*@ main :: () => {void | true} */
function main(){
	var n :number= pos();
	var m :number= sumLoop(0, n);
	assert(m === n);
}
