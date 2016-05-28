
function buildName(firstName: string, lastName?: string) {
    if (arguments.length === 2)
        return firstName + " " + lastName;
    else
        return firstName;
}

var result1 = buildName("Bob");
var result3 = buildName("Bob", "Adams");
