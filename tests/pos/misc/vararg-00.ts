/*@ plus :: /\ (number, number) => number 
            /\ (string, string) => string */
function plus(x, y) {
    
    var z = x + y;
    return z;
}


/*@ buildName :: /\ (firstName: string, lastName: number) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName, lastName) {
    if (typeof(lastName) === "number")
        return firstName;
   
    var a = plus (" ", lastName);
    var b = plus(firstName, a);
    return b;
}

var a = buildName("Bob", 56);              //works correctly now
var b = buildName("Bob", "Adams");         //ah, just right
