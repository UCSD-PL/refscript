/*@ concat :: /\ (string, string) => string 
              /\  (number, number) => number
*/

function concat(x, y) {
    
    var z = x + y;
    return z;
}


/*@ buildName :: /\ (firstName: string, lastName: number) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName, lastName) {
    if (typeof(lastName) === "number")
        return firstName;
   
    var a = concat (" ", lastName);
    var b = concat(firstName, a);
    return b;
}

var a = buildName("Bob", 12);         //works correctly now
var b = buildName("Bob", "Adams");      //ah, just right
