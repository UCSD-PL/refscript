
/*@ qualif EqM1(v: int): v = -1 */

/*@ foo :: <V extends posint, W extends number>(x: W, y: V, z: V, w: V) => W */
export function foo<V extends posint, W extends number>(x: W, y: V, z: V, w: V): W {
    return x;
}

let negone = -1;

let a_20_0 = foo(negone,2, 2, 2);

assert(a_20_0 === negone);
