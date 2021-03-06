/*@ plus :: (string, string) => string */
/*@ plus :: (number, number) => number */
function plus(x:number, y:number) {
    return x + y;
}


/*@ buildName :: (firstName: string, lastName: number) => string */
/*@ buildName :: (firstName: string, lastName: string) => string */

function buildName(firstName:any, lastName:any) : string {
    if (typeof(lastName) === "string")
        return firstName;

    let a = plus (" ", lastName);
    let b = plus(firstName, a);
    return b;
}

let a = buildName("Bob", 12);         //works correctly now
let b = buildName("Bob", "Adams");      //ah, just right
