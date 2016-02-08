/*@ plus :: /\  (string, string) => string 
            /\  (number, number) => number 

*/
function plus(x, y) {
    return x + y;
}


/*@ buildName :: /\ (firstName: string, lastName: number) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName:any, lastName:any) : string {
    if (typeof(lastName) === "number")
        return firstName;
  
    let a = plus (" ", lastName);
    let b = plus(firstName, a);
    return b;
}

let a = buildName("Bob", 12);         //works correctly now
let b = buildName("Bob", "Adams");      //ah, just right
