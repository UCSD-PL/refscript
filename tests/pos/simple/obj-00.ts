
/*@ y :: { } */
declare var y; 

/*@ x :: [#Mutable]{ f: { number | v > 0 } } */
var x = { f: 1 };
 
/*@ foo :: () => {void | true } */
function foo():void {
    x.f = 2;
}

/*@ main :: () => {void | true } */
function main () :void{
  foo();
}

