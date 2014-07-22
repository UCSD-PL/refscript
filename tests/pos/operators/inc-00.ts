/*@ qualif PlusOne(v:number, x:number)   : v = x + 1    */    

/*@ inc :: (number) => number */
function inc(x:number):number{
	var res:number = x + 1;
	return res;
}

/*@ main :: () => void */
function main():void{
	var a :number= pos();
	var b :number= inc(a);
	assert (b === (a + 1));
	assert(b > 0);
}

