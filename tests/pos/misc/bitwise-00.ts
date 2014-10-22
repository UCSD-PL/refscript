/*@ randomN :: (n:{number | 0 <= n}) => {v:number | 0 <= v && v <= n} */
function randomN(n:number){
    var r = Math.random() * n;
    // We just SUPPORT the operator with no fancy semantics;
    // the assume ensures that typechecking passes.
    r = r | 0;
    assume (0 <= r && r <= n);
    return r;
}
