function id<A>(x: A): A {
    return x;
}

export function foo(y: number): number;
export function foo(y: boolean): boolean;
export function foo(y: string): string;
export function foo(y: any): any {
    let z = id(y);
    return z;
}
