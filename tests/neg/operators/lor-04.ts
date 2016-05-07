
/*@ foo :: (x:null, y:number, z: number) => posint */
export function foo(x, y, z): number {

    let a = 1 || true;


    if (x || y || z) {
        return 1;
    }
    else {
        return 0;
    }



}
