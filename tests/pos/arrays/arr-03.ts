/*@ qualif OkLen(v:number, arr:a): v < (len arr) */

/*@ indirectIndex :: ( dataArr: #Array[#Immutable,number]
                     , idxArr : #Array[#Immutable,{number|(0 <= v && v < (len dataArr))}]
                     , i: { number | ((0 <= v) && (v < (len idxArr)))}) 
                     => number */
function indirectIndex(dataArr : number[], idxArr:number[], i:number) : number {
	var j = idxArr[i];
	return dataArr[j];
}
