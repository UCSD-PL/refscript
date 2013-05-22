function adpcm(n){
  requires(n > 0);
  var nsamples   = 2 * n;
  var inp0       = 0;
  var outp0      = 0;
  var inp        = inp0;
  var outp       = outp0;
  var bufferstep = 1;
  var len        = nsamples;
  while (0 < len) {
    invariant((inp - inp0) == (nsamples - len));
    invariant(inp0 <= inp);
    invariant(outp0 <= outp);
    invariant((2 * (outp - outp0)) == ((bufferstep - 1) + nsamples - len)) ;
    invariant(bufferstep == 0 || bufferstep == 1);
    assert((inp0 <= inp) && (inp  <  inp0 + nsamples));
    inp = inp + 1;
    if (bufferstep == 0){
      assert ((2 * (outp - outp0)) <= (nsamples + 1)); 
      outp = outp + 1;
    } 
    bufferstep = 1 - bufferstep;
    len = len - 1;
  }
  return 0;
}


