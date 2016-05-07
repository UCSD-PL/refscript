
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

declare function bak<A>(yyyy: A): string;
declare function bak(xxxx: number): number;
