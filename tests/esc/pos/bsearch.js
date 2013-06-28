//rjhala@goto:~/research/csolve/tests/postests (master)$ more adpcmini2.c
//#include <stdlib.h>
//#include <csolve.h>
//
///* TODO:
// * bufferstep toggle
// */
//
//number main()
//{
//  number len;
//  number bufferstep;
//  number n, nsamples;
//
//  /* 
//  number *inp;
//  number *outp;
//  number *inp0;
//  number *outp0;
//   */
//  
//  number inp;
//  number outp;
//  number inp0;
//  number outp0;
//  
//  n = nondetpos(); 
//  nsamples = 2*n;
//
//  inp0  = 0;//(number *)malloc(2*n);
//  outp0 = 0;//(number *)malloc(n);
//  
//  inp  = inp0;
//  outp = outp0;
//
//  bufferstep = 1;
//  
//  len = nsamples;
//  
//  for(; 0 < len; len--)
//  {
//    if ((inp - inp0) != nsamples - len){ csolve_assert(0); }
//    // csolve_assert(0); SANITY 
//    inp++;
//    if (len <= 0){ csolve_assert(0); }
//    if (2 * (outp - outp0) != (bufferstep-1) + nsamples - len) {csolve_assert(0);} 
//    //if (2 * (outp - outp0) > nsamples + 1){ csolve_assert(0); }
//
//    if (bufferstep == 0){
//      //access *outp
//      outp++;
//    } else {
//    }
//    bufferstep = 1 - bufferstep;
//  }
//  return 0;
//}
//
