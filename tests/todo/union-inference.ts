function noop(u) {}
/*@ foo :: (u:boolean + null) => {number | true} */
function foo(u) {
    noop(u); 
    return "not a number"; // UNSAFE
}