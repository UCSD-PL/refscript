/*@ qualif PLusOne(v:number, w: number)     : v = w + 1   */
/*@ inc :: (n : number) => {number | v = n+1}             */
function inc(n:number):number {
    return n + 1;
}

/*@ gobj :: { a: { v: number | v = 5 } 
            , b: string
            , f: (n:number) => { number | v = n + 1 } } */
var gobj= { a: 5, b: "String", f: inc };

/*@ gobj1 :: { a: number, b: string } */
var gobj1= { a: 5, b: "String", f: inc };

/*@ gobj2 :: { a: number } */
var gobj2= { a: 5, b: "String", f: inc };

/*@ foo :: () => { number | v = 6 } */
function foo() :number {

    var ff = gobj.f;
  
    return ff(gobj.a);
}
