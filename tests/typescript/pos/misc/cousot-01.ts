
/*@ loop :: (n:number, {m:number | m = n}) => void */
function loop(n:number, m:number) :void{
   
	if (random() > 0){
		n = n + 1;
		m = m + 1;
	}
	else {
		n = 0;
		m = 0;
	}
	
	assert(m == n);
	
	return;
}
