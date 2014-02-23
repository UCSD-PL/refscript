declare function empty (a : any[]) : boolean;
declare function head (a : any[]) : any;

/*@ hop :: (list [{v:number| 0 <= v}] + null) => void */
function hop(as : number[]) : void {
  if (empty(as)) {
    return;
  }
  var h = head(as);
  //assert(0 <= h);
  // var t = tail(xs);
  // return hop(t);
}

