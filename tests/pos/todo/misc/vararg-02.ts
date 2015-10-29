

/*@ buildName :: /\ (firstName: string) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName: string, lastName?: string) {
    if (lastName)
        return firstName + " " + lastName;
    else
        return firstName;
}

let result1 = buildName("Bob");                  //works correctly now
let result3 = buildName("Bob", "Adams");         //ah, just right
