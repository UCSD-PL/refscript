/*@ concat :: /\ (string, string) => string 
              /\  (number, number) => number
*/

function concat(x, y) { 
    let z = x + y;
    return z;
}


/*@ buildName :: /\ (firstName: string, lastName: number) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName, lastName) {
    if (typeof(lastName) === "number")
        return firstName;
   
    let a = concat (" ", lastName);
    let b = concat(firstName, a);
    return b;
}

let a = buildName("Bob", 12);         //works correctly now
let b = buildName("Bob", "Adams");      //ah, just right
