
export function minIndex(arr: IArray<number>): number {
    if (arr.length <= 0) return -1;
    let min = 0;

    // Try making the array accesses unsafe, e.g. by changing < to <=

    for (let i = 0; i < arr.length; i++) {
        let cur = arr[i];
        if (cur < arr[min]) {
            min = i;
        }
    }
    return min;
}
