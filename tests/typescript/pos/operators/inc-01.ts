
/*@ inc :: (number) => void */
function inc(x:number):void{
	assume(x > 0);
	var y :number= x + 1;
	assert(y > 0);
}

