/*@ qualif OkLen(v:number, arr:a): v < (len arr) */
/*@ indirectIndex :: ( dataArr:[number]
, idxArr:[{number|(0 <= v && v < (len dataArr))}]
, i: { number | ((0 <= v) && (v < (len idxArr)))})
=> number */
function indirectIndex(dataArr, idxArr, i) {
    var j = idxArr[i];
    return dataArr[j];
}
