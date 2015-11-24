
/*@ foo :: (o: IArray<number>) => {number | 0 < 1} */
function foo(o) {
    var ret = 0;

    for (var k in o) {
      ret += o[k];
    }  

    return ret;
};
