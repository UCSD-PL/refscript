
function partition(arr, begin, end, pivot)
{
    var piv=arr[pivot];
    swap(arr, pivot, end-1);
    var store=begin;
    var ix;
    for(ix=begin; ix<end-1; ++ix) {
        if(arr[ix]<=piv) {
            swap(arr,store, ix);
            ++store;
        }
    }
    swap(arr,end-1, store);

    return store;
}

function swap(arr, a, b)
{
    var tmp=arr[a];
    arr[a]=arr[b];
    arr[b]=tmp;
}

function qsort(arr, begin, end)
{
    if(end-1>begin) {
        var pivot=begin

        pivot=partition(arr, begin, end, pivot);

        qsort(arr, begin, pivot);
        qsort(arr, pivot+1, end);
    }
    return arr
}

function quick_sort(arr)
{
    return qsort(arr, 0, arr.length);
}

stuff = [1,77,-2,13,-12,3,9,11,0,4,5,-3,8,3,3,7]
var out = quick_sort(stuff)
out[0] //-12

