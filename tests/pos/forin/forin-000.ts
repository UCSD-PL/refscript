/*@ qualif UBound(v:number, x:a) : v < (len x) */


/*@ foo :: (o: #Array[#Immutable,number]) => {number | true} */
function foo(o) {
    var ret = 0;

    for (var k in o) {
      ret += o[k];
    }  

    return ret;
};
