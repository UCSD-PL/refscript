
/*@ qualif PlusOne(v:number, w: number): v = w + 1 */
/*@ qualif Six(v:number): v = 6 */
/*@ qualif PlusTwo(v:number, w: number): v = w + 2 */
/*@ qualif Seven(v:number): v = 7 */

function inc1(n: number) { return n + 1; }
function inc2(n: number) { return n + 2; }

/*@ readonly */
let x = { f: inc1 };

module A {

    function foo() :number {
      return (x.f)(5);
    }

    x.f = inc2;

    assert(foo() === 7);

}
