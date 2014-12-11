/*@ qualif CondLock1(v:number,x:number): v = (if (0 < x) then 1 else 0)  */    

/*@ create :: () => {number | v = 0} */
function create():number{
  return 0;
}

/*@ acquire :: ({number | v = 0}) => {number | v = 1} */
function acquire(l:number):number{
  return 1;
}

/*@ release :: ({number | v != 0}) => {number | v = 0} */
function release(l:number):number{
  return 0;
}

/*@ loop :: (n:number, l:{number | v = 0}) => {number | v = 0} */
function loop(n:number, l:number):number {
	var flag :number= random();
	if (0 < n){
		if (0 < flag){ 
			l = acquire(l); 
		}
		if (0 < flag){ 
			l = release(l);
		}
		loop(n-1, l);
	}
	return l;
}

/*@ main :: ({n:number|n > 0}) => {void | true} */
function main(n:number):void{
	var l:number= create();
	loop(n, l);
}

