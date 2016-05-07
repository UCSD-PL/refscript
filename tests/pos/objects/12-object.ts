
/*@ foo :: ( (Mutable){x: number}) => void */
export function foo(o):void {
    o.x = 10;
}
