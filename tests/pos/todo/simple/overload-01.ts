


function foo(x: string): string;
function foo(x: number): number;
function foo(x: any): any {
    if (typeof x === "string") {
        return "-" + x;
    }
    else {
        return - x;
    }
}

function bar(x: number): number;
function bar(x: any): any {
    if (typeof x === "string") {
        return "-" + x;
    }
    else {
        return - x;
    }
}

module M {
    module N {
        declare function inmodulebaz(x: string): string;
        declare function inmodulebaz(x: number): number;
    }

    declare function notexported(x: string): string;

    export declare function exported(x: number): number;
    export declare function exported(x: string): string;

    declare function notexported(x: number): number;

}

declare function bak<A>(x: A): string;
declare function bak(x: number): number;

interface IA {

    interfacefoo(x: number): number;
    interfacefoo(x: string): string;
    interfacefoo(x: boolean): boolean;
    interfacefoo(x: any): any;

    field: (x: number) => number;
}

class A {

    constructor() { }

    classfoo(x: number): number;
    classfoo(x: string): string;
    classfoo(x: boolean): boolean;
    classfoo(x: any): any {
        if (typeof x === "number") return 1;
        else if (typeof x === "string") return "a";
        else return true;
    }

    static sssss(x: number): number;
    static sssss(x: string): string;
    static sssss(x: boolean): boolean;
    static sssss(x: any): any {
        if (typeof x === "number") return 1;
        else if (typeof x === "string") return "a";
        else return true;
    }

    BBBBBB: (x: number) => number =
    function(x: number)
    /*@ <anonymous> (number) => number */
    { return 1; };
}


var a: {
    objectfoo(x: number): number;
    objectfoo(x: string): string;
    objectfoo(x: boolean): boolean;
    objectfoo(x: any): any;
};

var s = M.exported(1);
