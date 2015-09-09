
interface Error {
    name: string;
    message: string;
}

interface ErrorConstructor {
    new (message?: string): Error;
    (message?: string): Error;
    prototype: Error;
}

declare var Error: ErrorConstructor;
