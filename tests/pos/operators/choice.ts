//THIS IS FINE

/*@ bob :: (x:number) => {v:number | v != 0} */
function bob(x:number):number {
    if (x) {
	    return x;
    } else { 
	    return (x + 1);
    }
}

// BUT this is not -- and should be treated like the above...

/*@ main :: (x:number) => {v:number | v != 0} */
function main(x:number): number {
  return x ? x : (x + 1);
}

