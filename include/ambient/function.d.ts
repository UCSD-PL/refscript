
/*** Function ************************************************************/

interface Function {

    apply(thisArg: any, argArray?: any): any;

    // TODO: restore when rest parameters are supported
    call(thisArg: any): any;
    call(thisArg: any, a1: any): any;
    call(thisArg: any, a1: any, a2: any): any;
    call(thisArg: any, a1: any, a2: any, a3: any): any;
    call(thisArg: any, a1: any, a2: any, a3: any, a4: any): any;
    call(thisArg: any, a1: any, a2: any, a3: any, a4: any, a5: any): any;
    call(thisArg: any, a1: any, a2: any, a3: any, a4: any, a5: any, a6: any): any;
    call(thisArg: any, a1: any, a2: any, a3: any, a4: any, a5: any, a6: any, a7: any): any;

    // bind<M extends ReadOnly>(thisArg: any, ...argArray: Array<M, any>): any;

    prototype: any;
    length: number;

    // Non-standard extensions
    arguments: any;
    caller: Function;
}

declare let Function: {
    //new (...args: string[]): Function;
    //(...args: string[]): Function;
    prototype: Function;
}
