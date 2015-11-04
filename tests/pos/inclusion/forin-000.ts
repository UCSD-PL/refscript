
/*@ option --extrainvs */

/*@ foo :: (o: IArray<number>) => {number | 1 > 0} */
function foo(o) {
    var ret = 0;

    for (var k in o) {
      ret += o[k];
    }  

    return ret;
};
