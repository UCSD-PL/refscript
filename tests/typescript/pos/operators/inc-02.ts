
/*@ inc :: ({x:number|true}) => {v:number | v = x+1} */
function inc(x:number):number{
	var res :number= x + 1;
	return res;
}

/*@ main :: () => void */
function main():void{
	var a :number= pos();
	var b :number= inc(a);
	assert (b === (a + 1));
	assert(b > 0);
}

