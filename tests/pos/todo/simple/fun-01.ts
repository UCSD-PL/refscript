
/*@ qualif PlusOne(v:number, x: number): v = x + 1 */
/*@ qualif PlusTwo(v:number, x: number): v = x + 2 */

/*@ readonly bar :: # */
var bar = function (x: number): number 
/*@ <anonymous> (x: number) => number */
{
  return x + 1;
}

/*@ readonly foo :: # */
var foo = function (x: number): number 
/*@ <anonymous> (x: number) => number */
{
  var a = bar(x);
  var b = bar(a);
  return b;
}

var baz = foo;

assert(bar(1) === 2);

assert(foo(1) === baz(1));
