/*@ qualif OkLen(v:number, arr:a): v < (len arr) */

/*@ indirectIndex :: ( dataArr:[number]
                     , idxArr:[{number|(0 <= v && v < (len dataArr))}]
                     , i: { number | ((0 <= v) && (v < (len idxArr)))}) 
                     => number */
function indirectIndex(dataArr : number[], idxArr:number[], i:number) : number {
	var j : number= idxArr[i];
	return dataArr[j];

}
