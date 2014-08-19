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
    if (typeof(lastName) === "string")
        return firstName;
  
    var a = plus (" ", lastName);
    var b = plus(firstName, a);
    return b;
}

var a = buildName("Bob", 12);         //works correctly now
var b = buildName("Bob", "Adams");      //ah, just right
