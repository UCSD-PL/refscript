

/*@ buildName :: (firstName: string, lastName: undefined) => string */
/*@ buidName :: (firstName: string, lastName: string   ) => string */

function buildName(firstName: string, lastName?: string) {
    if (firstName)
        return firstName + " " + lastName;
    else
        return firstName;
}

let result1 = buildName("Bob");                  //works correctly now
let result3 = buildName("Bob", "Adams");         //ah, just right
let result2 = buildName("Bob", "Adams", "Sr.");  //error, too many parameters
