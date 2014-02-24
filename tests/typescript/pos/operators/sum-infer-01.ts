
// Ha ha. Why is this safe? :)

/*@ sumLoop :: (number, number) => number */
function sumLoop(acc:number, i:number){
	var r :number= acc;
	
	if (0 < i){
		r = sumLoop(acc + 1, i);
	} 
	
	return r;
}

/*@ main :: () => void */
function main(){
	var n:number = pos();
	var m:number = sumLoop(0, n);
	assert(m == n);
}
