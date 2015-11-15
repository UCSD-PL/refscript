
/*@ qualif HasP<A>(x: string, y: A): hasProperty(x, y) */

/* qualif EnumP<A>(v:A, s:B): enumProp(v,s)    */

// /*  values :: forall T . (mp: [Immutable]{ [k:string]: T }) =>  { void | 0 < 1 } */
// function values<T>(mp: { [k: string]: T }): void {
//     for (var key in mp) {
//         var a = <T>mp[key];
//     }
// };

/* fff :: forall A . (x: [Immutable] { [k:string]: A }, n: number) => { IArray<{ v: string | v = n }> | len v = 1 } */
// declare function fff(m, n): string[];
// var xxx = fff(mp, 1);

/*@ measure hasp :: forall A . (string, A) => bool */

/*@ mmm :: [Immutable]{ [k:string]: number } */
declare var mmm;

/*@ a :: IArray<number> */
declare var a: number[];


/*@ xxx :: { IArray<{ v: string | len a = 1 }> | len v = 10 } */
declare var xxx: number[];
var r = xxx[0];



/* bracket :: forall A . (m: [Immutable] {[y: string]: A }, k: string) => { v: A | hasp k m } + { undefined | not (hasp k m) } */
// declare function bracket<A>(m: { [y: string]: A }, k: string): A;
