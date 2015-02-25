
/*@ foo :: (o: IArray<number>) => {number | true} */
function foo(o) {
    var ret = 0;

    for (var k in o) {
      ret += o[k];
    }  

    return ret;
};
