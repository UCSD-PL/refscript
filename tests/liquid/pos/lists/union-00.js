/*@ qualif Len1(v:a)               : 0 <= (len xs)                 */
/*@ qualif Len2(v:a)               : 0 <  (len xs)                 */

/*@ foo :: forall A. (list[A] + null) => number */
function foo(xs){
    assert(length(xs) > 0);
    var zs = xs;
    return 12;
}

/*@ main :: () => {void | true} */
function main(){
    var a = nil();
    var b = cons(1, a);
    foo(b);
}


