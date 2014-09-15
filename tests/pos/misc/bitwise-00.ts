
/*@ randomN :: (n:{number | 0 <= n}) => {v:number | 0 <= v && v <= n} */
function randomN(n:number){
    var r = Math.random() * n;
    // this next line triggers a crash. I'm fine to just SUPPORT the operator, 
    // no need for any fancy semantics, since the assume ensures that typechecking passes.
    r = r | 0;
    assume (0 <= r && r <= n);
    return r;
}

