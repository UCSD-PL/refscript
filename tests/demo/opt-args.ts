
function buildName(firstName: string, lastName?: string) {
    if (arguments.length === 2)
        return firstName + " " + lastName;
    else
        return firstName;
}

let result1 = buildName("Bob");
let result3 = buildName("Bob", "Adams");
