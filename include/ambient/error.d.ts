
interface Error<M extends Immutable> {
    name: string;
    message: string;
}

interface ErrorConstructor<M extends Immutable> {
    new (message?: string): Error<M>;
    (message?: string): Error<M>;
    prototype: Error<M>;
}

declare var Error: ErrorConstructor<Immutable>;
