
export function foo(o: IArray<number>): number {
    let ret = 0;

    for (let k in o) {
      ret += o[k];
    }

    return ret;
};
