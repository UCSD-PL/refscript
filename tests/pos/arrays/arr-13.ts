
export function foo<A>(a: MArray<A>, e: A): MArray <A> {
    a.push(e);
    return a;
}
