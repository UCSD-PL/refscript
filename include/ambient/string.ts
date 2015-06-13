
/**
 *  STRING
 *  
 *  https://github.com/Microsoft/TypeScript/blob/master/src/lib/core.d.ts#L259
 *
 */

interface String {

    /*@ __proto__ : [Immutable] StringConstructor<Immutable> */
    __proto__: StringConstructor;

    toString(): string;

    charAt(pos: number): string;

    charCodeAt(index: number): number;

    // concat(...strings: string[]): string;

    indexOf(searchString: string, position?: number): number;

    lastIndexOf(searchString: string, position: number): number;

    localeCompare(that: string): number;

    match(regexp: string): string[];

    // match(regexp: RegExp): string[];

    replace(searchValue: string, replaceValue: string): string;

    // replace(searchValue: string, replaceValue: (substring: string, ...args: any[]) => string): string;

    // replace(searchValue: RegExp, replaceValue: string): string;

    // replace(searchValue: RegExp, replaceValue: (substring: string, ...args: any[]) => string): string;

    search(regexp: string): number;

    // search(regexp: RegExp): number;

    slice(start?: number, end?: number): string;

    split(separator: string, limit?: number): string[];

    // split(separator: RegExp, limit?: number): string[];

    substring(start: number, end?: number): string;

    toLowerCase(): string;

    toLocaleLowerCase(): string;

    toUpperCase(): string;

    toLocaleUpperCase(): string;

    trim(): string;

    /*@ length: { number | v >= 0 } */
    length: number;

    substr(from: number, length?: number): string;

    // [index: number]: string;
}

/*@ String :: StringConstructor<Immutable> */
declare var String /*@ readonly */: StringConstructor;

interface StringConstructor {
    new (value?: any): String;
    (value?: any): string;
    prototype: String;
    // fromCharCode(...codes: number[]): string;
    fromCharCode(code: number): string;
}

