
/*@ init :: (number) => {v: number | 0 <= v} */
function init(n:number):number{
    var i:number = undefined;
    var sum = 0; 
    while (n > 0){
	i = n;
	sum += i;
    }
    return sum;
}