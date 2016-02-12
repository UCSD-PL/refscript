/*@ qualif Len1(v:a)               : 0 <= (len v)                 */
/*@ qualif Len2(v:a)               : 0 <  (len v)                 */

/*@ foo :: <A,M extends ReadOnly> ({LList<M,A> | LLlen(v) > 0}) => number */
function foo(xs){
    assert(false);
    let zs = xs;
    return 12;
}

/*@ main :: () => void */
function main(){
    let a = nil();
    let b = cons(1, a);
    foo(b);
}

main();
