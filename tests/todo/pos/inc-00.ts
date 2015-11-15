
// We should get this to FAIL
// inc :: ({number | 0 < 1 }) => void /

export function inc(x:number): void {
  var y = x + 1;
  assert(y > 0);
}

