
/*@ type e  {  next : e } */

/*@ type a  {  next : b } */
/*@ type b  {  next : c } */
/*@ type c  {  next : a } */


/*@ foo :: (x: a) => e */
function foo(x) {
    return x;
}

