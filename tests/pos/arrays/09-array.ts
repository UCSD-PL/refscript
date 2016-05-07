/*@ indirectIndex :: ( dataArr: IArray<number>
                     , idxArr : IArray<{number|(0 <= v && v < (len dataArr))}>
                     , i: { number | ((0 <= v) && (v < (len idxArr)))})
                     => number */
export function indirectIndex(dataArr : number[], idxArr:number[], i:number) : number {
	let j = idxArr[i];
	return dataArr[j];
}
