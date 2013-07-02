
/*@ type e  {  next : e } */

/*@ type a  {  next : b } */
/*@ type b  {  next : c } */
/*@ type c  {  next : number } */


/*@ foo :: (x: a) => e */
function foo(x) {
    return x;
}

