

/*@ buildName :: /\ (firstName: string) => string
                 /\ (firstName: string, lastName: string) => string
 */

function buildName(firstName: string, lastName?: string) {
    let zog = arguments.length;
    
    assert (zog > 0);

    if (arguments.length === 2)
        return firstName + " " + lastName;
    else
        return firstName;
}

let result1 = buildName("Bob");                  //works correctly now
let result3 = buildName("Bob", "Adams");         //ah, just right
