
/*@ qualif Bot  (v: a           ): 0 = 1        */
/*@ qualif Bot  (v: bool        ): 0 = 1        */
/*@ qualif CmpZ (v: int         ): v < 0        */
/*@ qualif Bot  (v: int         ): 0 = 1        */
/*@ qualif CmpZ (v: int         ): v <= 0       */
/*@ qualif CmpZ (v: int         ): v >  0       */
/*@ qualif CmpZ (v: int         ): v >= 0       */
/*@ qualif CmpZ (v: int         ): v =  0       */
/*@ qualif CmpO (v: int         ): v =  1       */
/*@ qualif CmpZ (v: int         ): v != 0       */
/*@ qualif Cmp  (v: int , x: int): v <  x       */
/*@ qualif Cmp  (v: int , x: int): v <= x       */
/*@ qualif Cmp  (v: int , x: int): v >  x       */
/*@ qualif Cmp  (v: int , x: int): v >= x       */
/*@ qualif Cmp  (v: a   , x: a  ): v ~~ x       */
/*@ qualif Cmp  (v: a   , x: a  ): v != x       */
/*@ qualif True (v: bool        ): (Prop v)     */
/*@ qualif False(v: bool        ): not (Prop v) */
/*@ qualif Tag  (v: Str , x: a  ): v = ttag x   */
/*@ qualif Len  (v: int , x: a  ): v < len w    */
