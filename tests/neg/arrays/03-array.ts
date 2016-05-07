/*@ indirectIndex :: ( dataArr: IArray<number>
                     , idxArr : IArray<idx<dataArr>>
                     , i: { number | 0 <= v && v <= len idxArr})
                     => number */
function indirectIndex(dataArr : number[], idxArr:number[], i:number) : number {
	let j = idxArr[i];
	return dataArr[j];
}
