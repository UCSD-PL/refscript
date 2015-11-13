
/*@ y :: { } */
declare var y; 

/*@ x :: [#Mutable]{ f: [#Mutable] { number | v > 0 } } */
var x = { f: 1 };
 
/*@ foo :: () => {void | 0 < 1 } */
function foo():void {
    x.f = 2;
}

/*@ main :: () => {void | 0 < 1 } */
function main () :void{
  foo();
}

