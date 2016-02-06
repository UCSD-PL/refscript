
export function foo(x: string): string;
export function foo(x: number): number;
export function foo(x: any): any {
    if (typeof x === "string") {
        return "-" + x;
    }
    else {
        return - x;
    }
}

export function bar(x: number): number;
export function bar(x: any): any {
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

interface IA<M extends ReadOnly> {

    interfacefoo(x: number): number;
    interfacefoo(x: string): string;
    interfacefoo(x: boolean): boolean;
    interfacefoo(x: any): any;

    field: (x: number) => number;
}

class A<M extends ReadOnly> {

    constructor() { }

    classfoo(x: number): number;
    classfoo(x: string): string;
    classfoo(x: boolean): boolean;
    classfoo(x: any): any {
        if (typeof x === "number") return 1;
        else if (typeof x === "string") return "a";
        else return true;
    }

    static _static(x: number): number;
    static _static(x: string): string;
    static _static(x: boolean): boolean;
    static _static(x: any): any {
        if (typeof x === "number") return 1;
        else if (typeof x === "string") return "a";
        else return true;
    }

}


let s = M.exported(1);
