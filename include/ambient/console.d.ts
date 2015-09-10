
/*** Console ************************************************************/
interface Console {
    info(message?: any, ...optionalParams: any[]): void;
    warn(message?: any, ...optionalParams: any[]): void;
    error(message?: any, ...optionalParams: any[]): void;
    log(message?: any, ...optionalParams: any[]): void;
    profile(reportName?: string): void;
    assert(test?: boolean, message?: string, ...optionalParams: any[]): void;
    //msIsIndependentlyComposed(element: Element): boolean;
    clear(): void;
    dir(value?: any, ...optionalParams: any[]): void;
    profileEnd(): void;
    count(countTitle?: string): void;
    groupEnd(): void;
    time(timerName?: string): void;
    timeEnd(timerName?: string): void;
    trace(): void;
    group(groupTitle?: string): void;
    dirxml(value: any): void;
    debug(message?: string, ...optionalParams: any[]): void;
    groupCollapsed(groupTitle?: string): void;
    //select(element: Element): void;
}
declare var Console: {
    prototype: Console;
    new(): Console;
}
declare var console: Console;
