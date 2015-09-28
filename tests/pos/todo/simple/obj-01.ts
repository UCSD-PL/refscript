

export function inc(n: number): void { }

/*@ gobj :: { a : { v: number | v = 5 }; f : (n:number) => void ; } */
var gobj = { a: 5, f: inc };
