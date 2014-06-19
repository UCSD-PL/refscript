

/*@ inc :: (n : number) => void */
function inc(n:number) { }

/*@ gobj :: { a: { v: number | v = 5 }; f: (n:number) => void ; } */
var gobj= { a: 5, f: inc };

