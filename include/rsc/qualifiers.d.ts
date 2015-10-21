
/*@ qualif Bot(v: a        ): 0 = 1  */
/*@ qualif Bot(v: obj      ): 0 = 1  */
/*@ qualif Bot(v: boolean  ): 0 = 1  */
/*@ qualif Bot(v: int      ): 0 = 1  */
/*@ qualif CmpZ(v:int      ): v < 0  */
/*@ qualif CmpZ(v:int      ): v <= 0 */
/*@ qualif CmpZ(v:int      ): v >  0 */
/*@ qualif CmpZ(v:int      ): v >= 0 */
/*@ qualif CmpZ(v:int      ): v =  0 */
/*@ qualif CmpO(v:int      ): v =  1 */
/*@ qualif CmpZ(v:int      ): v != 0 */
/*@ qualif Cmp(v:int, x:int): v <  x */
/*@ qualif Cmp(v:int, x:int): v <= x */
/*@ qualif Cmp(v:int, x:int): v >  x */
/*@ qualif Cmp(v:int, x:int): v >= x */

/*@ qualif Cmp(v:a,x:a): v ~~ x */
/*@ qualif Cmp(v:a,x:a): v != x */

/*@ qualif True1(v:boolean) : (Prop v)     */
/*@ qualif False1(v:boolean): not (Prop v) */
/*@ qualif Tag(v:a,x:string): ttag(v) = x  */

/*  qualif Cmp(v:a,x:a    ): v =  x  */
/*  qualif True(v:boolean ): v       */
/*  qualif False(v:boolean): (not v) */

// Somewhat more controversial qualifiers (i.e. "expensive"...)

/*  qualif Add(v:int,x:int,y:int): v = x + y */
/*  qualif Sub(v:int,x:int,y:int): v = x - y */

/*@  qualif Len(v:b,w:a)  : v < (len w) */
