
/*@ minIndex :: (arr:IArray<number>) => {number | 0 < 1} */
function minIndex(arr) {
    if (arr.length <= 0) return -1;

    let min = 0;
    for (let i = 0; i < arr.length; i++) {
        let cur = arr[i];
        if (cur < arr[min]) {
            min = i;
        }
    }
    return min;
}
