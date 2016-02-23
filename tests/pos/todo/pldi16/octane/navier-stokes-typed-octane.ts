/**
 * Copyright 2012 the V8 project authors. All rights reserved.
 * Copyright 2009 Oliver Hunt <http://nerget.com>
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/*@ qualif Add(v: number, n: number, m: number): v <= m + n */

/// <reference path="libs/mulThms.d.ts" />

module NavierStokes {
    /*@ solver :: {v:FluidField<Mutable> | offset(v,"width") = 128 && offset(v,"height") = 128} + null */
    let solver:FluidField<Mutable> = null;
    /*@ nsFrameCounter :: number */
    let nsFrameCounter = 0;

    export function runNavierStokes()
    {
        let solverRO /*@ readonly */ = solver;
        if (!solverRO) throw new Error("solver is null! did you forget to call setupNavierStokes first?");
        solverRO.update();
        nsFrameCounter++;

        if(nsFrameCounter===15)
            checkResult(solverRO.getDens());
    }

    /*@ checkResult :: ({v:IArray<number> | (len v) = (128+2) * (128+2)}) => void */
    function checkResult(dens) {
    
        let _lemma = mulThm130(130);
        let result = 0;
        for (let i=7000;i<7100;i++) {
            result = result + (dens[i]*10); //PORT TODO: result+=~~((dens[i]*10));
        }

        if (result!==74) {
            console.log("checksum failed: " + result);
        }
    }

    export function setupNavierStokes()
    {
        let _lemma = mulThm128(128);
        let solverRO /*@ readonly */ = new FluidField(null, 128, 128);
        solverRO.setIterations(20);

        let dFunc: (f:Field<Immutable>) => void = function() {};
        solverRO.setDisplayFunction(dFunc);
        solverRO.setUICallback(prepareFrame);
        solverRO.reset();
        solver = solverRO;
    }

    export function tearDownNavierStokes()
    {
        solver = null;
    }

    /*@ addPoints :: ({v:Field<Mutable> | offset(v,"w") = 128 && offset(v,"h") = 128}) => void */
    function addPoints(field:Field<Mutable>) {
        let n = 64;
        for (let i = 1; i <= n; i++) {
            field.setVelocity(i, i, n, n);
            field.setDensity(i, i, 5);
            field.setVelocity(i, n - i, -n, -n);
            field.setDensity(i, n - i, 20);
            field.setVelocity(128 - i, n + i, -n, -n);
            field.setDensity(128 - i, n + i, 30);
        }
    }

    /*@ framesTillAddingPoints :: number */
    let framesTillAddingPoints = 0;
    /*@ framesBetweenAddingPoints :: number */
    let framesBetweenAddingPoints = 5;

    /*@ prepareFrame :: ({v:Field<Mutable> | offset(v,"w") = 128 && offset(v,"h") = 128}) => void */
    function prepareFrame(field:Field<Mutable>)
    {
        if (framesTillAddingPoints === 0) {
            addPoints(field);
            framesTillAddingPoints = framesBetweenAddingPoints;
            framesBetweenAddingPoints++;
        } else {
            framesTillAddingPoints--;
        }
    }

    // Code from Oliver Hunt (http://nerget.com/fluidSim/pressure.js) starts here.
    export class FluidField<M extends ReadOnly> {
        /*@ (Immutable) width : pos */
        private width;
        /*@ (Immutable) height : pos */
        private height;
        /*@ (Immutable) rowSize : {v:number | v = this.width + 2} */
        private rowSize;
        /*@ (Immutable) size : {v:number | v = (this.height+2) * (this.width+2)} */
        private size;
        /*@ (Immutable) dens :      {v:IArray<number> | (len v) = this.size} */
        private dens;
        /*@ (Immutable) dens_prev : {v:IArray<number> | (len v) = this.size} */
        private dens_prev;
        /*@ (Immutable) u :         {v:IArray<number> | (len v) = this.size} */
        private u;
        /*@ (Immutable) u_prev :    {v:IArray<number> | (len v) = this.size} */
        private u_prev;
        /*@ (Immutable) v :         {v:IArray<number> | (len v) = this.size} */
        private v;
        /*@ (Immutable) v_prev :    {v:IArray<number> | (len v) = this.size} */
        private v_prev;
        private iters:number;
        private visc:number;
        private dt:number;
        private displayFunc: (f:Field<Immutable>) => void;

        /*@ uiCallback : ({v:Field<Mutable> | offset(v,"w") = this.width && offset(v,"h") = this.height}) => void */
        private uiCallback;

        /*@ new (canvas:top, 
                 hRes:pos, 
                 wRes:{v:pos | hRes * v < 1000000}) : {v:FluidField<M> | offset(v,"width") = wRes && offset(v,"height") = hRes} */
        constructor(canvas, hRes, wRes) {
            let width = wRes;
            let height = hRes;
            let size = (height+2) * (width+2);

            this.width = width;
            this.height = height;
            this.rowSize = width + 2;
            this.size = size;
            let dens      = new Array<number>(size);
            let dens_prev = new Array<number>(size);
            let u         = new Array<number>(size);
            let u_prev    = new Array<number>(size);
            let v         = new Array<number>(size);
            let v_prev    = new Array<number>(size);
            for (let i = 0; i < size; i++) {
                dens_prev[i] = 0; u_prev[i] = 0; v_prev[i] = 0; dens[i] = 0; u[i] = 0; v[i] = 0;
            }
            this.dens = dens;
            this.dens_prev = dens_prev;
            this.u = u;
            this.u_prev = u_prev;
            this.v = v;
            this.v_prev = v_prev;

            this.iters = 10;
            this.visc = 1/2;//.
            this.dt = 1/10;//.

            this.displayFunc = function(f:Field<Immutable>) 
                /*@ <anonymous> (Field<Immutable>)=>void */ 
                {}; //ORIG: null

            this.uiCallback = function(field:Field<Mutable>) 
                /*@ <anonymous> (Field<Mutable>)=>void */ 
                {};
        }

            /*@ addFields (x:{v:IArray<number> | (len v) = this.size}, 
                             s:{v:IArray<number> | (len v) = this.size}, 
                             dt:number) : void */
            addFields(x:number[], s:number[], dt:number)
            {
                for (let i=0; i<this.size; i++) x[i] = x[i] + dt*s[i]; //ORIG: +=
            }

            /*@ set_bnd (b:number, x:{v:IArray<number> | (len v) = this.size}) : void */
            set_bnd(b:number, x:number[])
            {
                let width   = this.width;
                let height  = this.height;
                let rowSize = this.rowSize;

                let _lemma0 = mulThm1(rowSize, height+2);

                let _lemma1 = mulThm2(rowSize, height, height+2);
                let _lemma2 = mulThm2(rowSize, height+1, height+2);

                if (b===1) {
                    for (let i = 1; i <= width; i++) {
                        x[i] =  x[i + rowSize];
                        x[i + (height+1) *rowSize] = x[i + height * rowSize];
                    }
                    for (let j = 1; j <= height; j++) {
                        let _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] = -x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] = -x[width + j * rowSize];
                    }
                } else if (b === 2) {
                    for (let i = 1; i <= width; i++) {
                        x[i] = -x[i + rowSize];
                        x[i + (height + 1) * rowSize] = -x[i + height * rowSize];
                    }

                    for (let j = 1; j <= height; j++) {
                        let _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] =  x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] =  x[width + j * rowSize];
                    }
                } else {
                    for (let i = 1; i <= width; i++) {
                        x[i] =  x[i + rowSize];
                        x[i + (height + 1) * rowSize] = x[i + height * rowSize];
                    }

                    for (let j = 1; j <= height; j++) {
                        let _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] =  x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] =  x[width + j * rowSize];
                    }
                }
                let maxEdge = (height + 1) * rowSize;
                x[0]                 = 1/2 * (x[1] + x[rowSize]);//.
                x[maxEdge]           = 1/2 * (x[1 + maxEdge] + x[height * rowSize]);//.
                x[(width+1)]         = 1/2 * (x[width] + x[(width + 1) + rowSize]);//.
                x[(width+1)+maxEdge] = 1/2 * (x[width + maxEdge] + x[(width + 1) + height * rowSize]);//.
            }

            /*@ lin_solve (b :number, 
                             x :{v:IArray<number> | (len v) = this.size}, 
                             x0:{v:IArray<number> | (len v) = this.size}, 
                             a :number, 
                             c :{v:number | v != 0}) : void */
            lin_solve(b:number, x:number[], x0:number[], a:number, c:number)
            {
                let width = this.width;
                let height = this.height;
                let rowSize = this.rowSize;

                if (a === 0 && c === 1) {
                    for (let j=1 ; j<=height; j++) {
                        let currentRow = j * rowSize;
                        let _lemma = mulThm2(rowSize, j, height+2);
                        ++currentRow;
                        for (let i = 0; i < width; i++) {
                            //needs Add2
                            x[currentRow] = x0[currentRow];
                            ++currentRow;
                        }
                    }
                    this.set_bnd(b, x);
                } else {
                    let invC = 1 / c;
                    for (let k=0 ; k<this.iters; k++) {
                        for (let j=1 ; j<=height; j++) {
                            let lastRow = (j - 1) * rowSize;
                            let currentRow = j * rowSize;
                            let nextRow = (j + 1) * rowSize;
                            let _lemma1 = mulThm2(rowSize, j-1, height+2);
                            let _lemma2 = mulThm2(rowSize, j, height+2);
                            let _lemma3 = mulThm2(rowSize, j+1, height+2);
                            let lastX = x[currentRow];
                            ++currentRow;
                            for (let i=1; i<=width; i++) {
                                x[currentRow] = (x0[currentRow] + a*(lastX+x[++currentRow]+x[++lastRow]+x[++nextRow])) * invC;
                                lastX = x[currentRow];
                            }
                        }
                        this.set_bnd(b, x);
                    }
                }
            }

            /*@ diffuse (b :number, 
                           x :{v:IArray<number> | (len v) = this.size}, 
                           x0:{v:IArray<number> | (len v) = this.size}, 
                           dt:number) : void */
            diffuse(b:number, x:number[], x0:number[], dt:number)
            {
                let a = 0;
                this.lin_solve(b, x, x0, a, 1 + 4*a);
            }

            /*@ lin_solve2 (x :{v:IArray<number> | (len v) = this.size}, 
                              x0:{v:IArray<number> | (len v) = this.size}, 
                              y :{v:IArray<number> | (len v) = this.size}, 
                              y0:{v:IArray<number> | (len v) = this.size}, 
                              a :number, 
                              c :{v:number | v != 0}) : void */
            lin_solve2(x:number[], x0:number[], y:number[], y0:number[], a:number, c:number)
            {
                let width = this.width;
                let height = this.height;
                let rowSize = this.rowSize;

                if (a === 0 && c === 1) {
                    for (let j=1 ; j <= height; j++) {
                        let currentRow = j * rowSize;
                        let _lemma = mulThm2(rowSize, j, height+2);
                        ++currentRow;
                        for (let i = 0; i < width; i++) {
                            //needs Add2
                            x[currentRow] = x0[currentRow];
                            y[currentRow] = y0[currentRow];
                            ++currentRow;
                        }
                    }
                    this.set_bnd(1, x);
                    this.set_bnd(2, y);
                } else {
                    let invC = 1/c;
                    for (let k=0 ; k<this.iters; k++) {
                        for (let j=1 ; j <= height; j++) {
                            let lastRow = (j - 1) * rowSize;
                            let currentRow = j * rowSize;
                            let nextRow = (j + 1) * rowSize;
                            let _lemma1 = mulThm2(rowSize, j-1, height+2);
                            let _lemma2 = mulThm2(rowSize, j, height+2);
                            let _lemma3 = mulThm2(rowSize, j+1, height+2);
                            let lastX = x[currentRow];
                            let lastY = y[currentRow];
                            ++currentRow;
                            for (let i = 1; i <= width; i++) {
                                x[currentRow] = (x0[currentRow] + a * (lastX + x[currentRow] + x[lastRow] + x[nextRow])) * invC;
                                lastX = x[currentRow];
                                y[currentRow] = (y0[currentRow] + a * (lastY + y[++currentRow] + y[++lastRow] + y[++nextRow])) * invC;
                                lastY = y[currentRow];
                            }
                        }
                        this.set_bnd(1, x);
                        this.set_bnd(2, y);
                    }
                }
            }

            /*@ diffuse2 (x :{v:IArray<number> | (len v) = this.size}, 
                            x0:{v:IArray<number> | (len v) = this.size}, 
                            y :{v:IArray<number> | (len v) = this.size}, 
                            y0:{v:IArray<number> | (len v) = this.size}, 
                            dt:number) : void */
            diffuse2(x:number[], x0:number[], y:number[], y0:number[], dt:number)
            {
                let a = 0;
                this.lin_solve2(x, x0, y, y0, a, 1 + 4 * a);
            }

            /*@ advect (b :number, 
                          d :{v:IArray<number> | (len v) = this.size}, 
                          d0:{v:IArray<number> | (len v) = this.size}, 
                          u :{v:IArray<number> | (len v) = this.size}, 
                          v :{v:IArray<number> | (len v) = this.size}, 
                          dt:number) : void */
            advect(b:number, d:number[], d0:number[], u:number[], v:number[], dt:number)
            {
                let width = this.width;
                let height = this.height;
                let rowSize = this.rowSize;

                let Wdt0 = dt * width;
                let Hdt0 = dt * height;
                let Wp5 = width + 1/2;//.
                let Hp5 = height + 1/2;//.
                for (let j = 1; j<= height; j++) {
                    let pos = j * rowSize;
                    let _lemma = mulThm2(rowSize, j, height+2);
                    for (let i = 1; i <= width; i++) {
                        let x:any = i - Wdt0 * u[++pos];
                        let y:any = j - Hdt0 * v[pos];
                        if (x < 1/2)//.
                            x = 1/2;//.
                        else if (x > Wp5)
                            x = Wp5;
                        let i0 = Math.floor(x); //ORIG: x | 0;
                        let i1 = i0 + 1;
                        if (y < 1/2)//.
                            y = 1/2;//.
                        else if (y > Hp5)
                            y = Hp5;
                        let j0 = Math.floor(y); //ORIG: y | 0;
                        let j1 = j0 + 1;
                        let s1 = x - i0;
                        let s0 = 1 - s1;
                        let t1 = y - j0;
                        let t0 = 1 - t1;
                        let row1 = j0 * rowSize;
                        let row2 = j1 * rowSize;
                        mulThm2(rowSize, j0, height+2);
                        mulThm2(rowSize, j1, height+2);
                        d[pos] = s0 * (t0 * d0[i0 + row1] + t1 * d0[i0 + row2]) + s1 * (t0 * d0[i1 + row1] + t1 * d0[i1 + row2]);
                    }
                }
                this.set_bnd(b, d);
            }

            /*@ project (u  :{v:IArray<number> | (len v) = this.size}, 
                           v  :{v:IArray<number> | (len v) = this.size}, 
                           p  :{v:IArray<number> | (len v) = this.size}, 
                           divv:{v:IArray<number> | (len v) = this.size}) : void */
            project(u:number[], v:number[], p:number[], divv:number[])
            {
                let width = this.width;
                let height = this.height;
                let rowSize = this.rowSize;

                let h = -(1/2) / Math.sqrt(width * height);//.
                for (let j = 1 ; j <= height; j++ ) {
                    let row = j * rowSize;
                    let previousRow = (j - 1) * rowSize;
                    let prevValue = row - 1;
                    let currentRow = row;
                    let nextValue = row + 1;
                    let nextRow = (j + 1) * rowSize;
                    let _lemma1 = mulThm2(rowSize, j-1, height+2);
                    let _lemma2 = mulThm2(rowSize, j, height+2);
                    let _lemma3 = mulThm2(rowSize, j+1, height+2);
                    for (let i = 1; i <= width; i++ ) {
                        divv[++currentRow] = h * (u[++nextValue] - u[++prevValue] + v[++nextRow] - v[++previousRow]);
                        p[currentRow] = 0;
                    }
                }
                this.set_bnd(0, divv);
                this.set_bnd(0, p);

                this.lin_solve(0, p, divv, 1, 4 );
                let wScale = 1/2 * width;//.
                let hScale = 1/2 * height;//.
                for (let k = 1; k<= height; k++ ) {
                    let prevPos = k * rowSize - 1;
                    let currentPos = k * rowSize;
                    let nextPos = k * rowSize + 1;
                    let prevRow = (k - 1) * rowSize;
                    let currentRow = k * rowSize;
                    let nextRow = (k + 1) * rowSize;
                    let _lemma1 = mulThm2(rowSize, k-1, height+2);
                    let _lemma2 = mulThm2(rowSize, k, height+2);
                    let _lemma3 = mulThm2(rowSize, k+1, height+2);
                    for (let i = 1; i<= width; i++) {
                        ++currentPos;
                        u[currentPos] = u[currentPos] - wScale * (p[++nextPos] - p[++prevPos]); //ORIG: -=
                        v[currentPos] = v[currentPos] - hScale * (p[++nextRow] - p[++prevRow]); //ORIG: -=
                    }
                }
                this.set_bnd(1, u);
                this.set_bnd(2, v);
            }

            /*@ dens_step (x :{v:IArray<number> | (len v) = this.size}, 
                             x0:{v:IArray<number> | (len v) = this.size}, 
                             u :{v:IArray<number> | (len v) = this.size}, 
                             v :{v:IArray<number> | (len v) = this.size}, 
                             dt:number) : void */
            dens_step(x:number[], x0:number[], u:number[], v:number[], dt:number)
            {
                this.addFields(x, x0, dt);
                this.diffuse(0, x0, x, dt );
                this.advect(0, x, x0, u, v, dt );
            }

            /*@ vel_step (u :{v:IArray<number> | (len v) = this.size}, 
                            v :{v:IArray<number> | (len v) = this.size}, 
                            u0:{v:IArray<number> | (len v) = this.size}, 
                            v0:{v:IArray<number> | (len v) = this.size}, 
                            dt:number) : void */
            vel_step(u:number[], v:number[], u0:number[], v0:number[], dt:number)
            {
                this.addFields(u, u0, dt );
                this.addFields(v, v0, dt );
                let temp = u0; u0 = u; u = temp;
                temp = v0; v0 = v; v = temp;
                this.diffuse2(u,u0,v,v0, dt);
                this.project(u, v, u0, v0);
                temp = u0; u0 = u; u = temp;
                temp = v0; v0 = v; v = temp;
                this.advect(1, u, u0, u0, v0, dt);
                this.advect(2, v, v0, u0, v0, dt);
                this.project(u, v, u0, v0 );
            }

            /*@ queryUI (d:{v:IArray<number> | (len v) = this.size}, 
                           u:{v:IArray<number> | (len v) = this.size}, 
                           v:{v:IArray<number> | (len v) = this.size}) : void */
            queryUI(d:number[], u:number[], v:number[])
            {
                for (let i = 0; i < this.size; i++) {
                    u[i] = 0; v[i] = 0; d[i] = 0;//.
                }
                this.uiCallback(new Field(this.rowSize, this.width, this.height, d, u, v));
            } 

            public update() 
            {
                this.queryUI(this.dens_prev, this.u_prev, this.v_prev);
                this.vel_step(this.u, this.v, this.u_prev, this.v_prev, this.dt);
                this.dens_step(this.dens, this.dens_prev, this.u, this.v, this.dt);
                this.displayFunc(new Field(this.rowSize, this.width, this.height, this.dens, this.u, this.v));
            }
            /*@ @Mutable setDisplayFunction ((Field<Immutable>)=>void) : void */
            public setDisplayFunction(f:(f:Field<Immutable>) => void) {
                this.displayFunc = f;
            }
            
            public iterations() { return this.iters; }
            /*@ @Mutable setIterations (iters:number) : void */
            public setIterations(iters:number) 
            {
                if (iters > 0 && iters <= 100)
                    this.iters = iters;
            }
            /*@ @Mutable setUICallback (({v:Field<Mutable> | offset(v,"w") = this.width && offset(v,"h") = this.height})=>void) : void */
            public setUICallback(callback:(f:Field<Mutable>) => void) {
                this.uiCallback = callback;
            }
            public reset()
            {
                for (let i = 0; i < this.size; i++) {
                    this.dens_prev[i] = 0; this.u_prev[i] = 0; this.v_prev[i] = 0; this.dens[i] = 0; this.u[i] = 0; this.v[i] = 0;
                }
            }
            /*@ getDens () : {v:IArray<number> | (len v) = this.size} */
            public getDens()
            {
                return this.dens;
            }
    }

    export class Field<M extends ReadOnly> {
        /*@ (Immutable) rowSize : {v:number | v = this.w + 2} */
        private rowSize;
        /*@ (Immutable) w : pos */
        private w;
        /*@ (Immutable) h : pos */
        private h;
        /*@ (Immutable) dens : {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private dens;
        /*@ (Immutable) u    : {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private u;
        /*@ (Immutable) v    : {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private v;

        /*@ new (rowSize: {v:number | v = w+2}, 
                 w:       pos, 
                 h:       pos, 
                 dens:    {v:IArray<number> | (len v) = (h+2) * (w+2)},
                 u:       {v:IArray<number> | (len v) = (h+2) * (w+2)},
                 v:       {v:IArray<number> | (len v) = (h+2) * (w+2)}) : {v:Field<M> | offset(v,"w") = w && offset(v,"h") = h } */
        constructor(rowSize:number, w:number, h:number, dens:number[], u:number[], v:number[]) {
            this.rowSize = rowSize;
            this.w = w;
            this.h = h;
            this.dens = dens;
            this.u = u;
            this.v = v;
        }

            /*@ setDensity (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}, d:number) : void */
            public setDensity(x:number, y:number, d:number) {
                let _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                this.dens[(x + 1) + (y + 1) * this.rowSize] = d;
            }
            /*@ getDensity (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getDensity(x:number, y:number) {
                let _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.dens[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ setVelocity (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}, xv:number, yv:number) : void */
            public setVelocity(x:number, y:number, xv:number, yv:number) {
                let _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                this.u[(x + 1) + (y + 1) * this.rowSize] = xv;
                this.v[(x + 1) + (y + 1) * this.rowSize] = yv;
            }
            /*@ getXVelocity (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getXVelocity(x:number, y:number) {
                let _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.u[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ getYVelocity (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getYVelocity(x:number, y:number) {
                let _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.v[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ width () : {v:number | v = this.w} */
            public width():number { return this.w; }
            /*@ height () : {v:number | v = this.h} */
            public height():number { return this.h; }
    }
}
