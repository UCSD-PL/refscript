
/*@ type a  {  next : b } */
/*@ type b  {  next : c } */
/*@ type c  {  next : a } */


/*@ foo :: (x: a) => c */
function foo(x) {
    return x;
}

