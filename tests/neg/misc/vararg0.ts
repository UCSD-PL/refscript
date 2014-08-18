

/*@ buildName :: /\ (firstName: string, lastName: undefined) => string
                 /\ (firstName: string, lastName: string   ) => string
 */

function buildName(firstName: string, lastName?: string) {
    if (firstName)
        return firstName + " " + lastName;
    else
        return firstName;
}

var result1 = buildName("Bob");                  //works correctly now
var result3 = buildName("Bob", "Adams");         //ah, just right


var result2 = buildName("Bob", "Adams", "Sr.");  //error, too many parameters