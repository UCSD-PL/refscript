/*@ qualif PLusOne(v:number, w: number)     : v = w + 1                            */
/*@ inc :: (n : number) => {number | v = n+1}  */
function inc(n:number):number {
    return n + 1;
}

var gobj= {
    a: 5,
    b: "String",
    f: inc
};

/*@ foo :: () => { number | v = 6 } */
function foo() :number{
    var ff :(number)=>number= gobj.f;
    return ff(gobj.a);
}
