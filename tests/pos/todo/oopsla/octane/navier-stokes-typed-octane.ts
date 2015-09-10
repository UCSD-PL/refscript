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

/*@ qualif Add(v: number, n: number, m: number): v < m + n */
/*@ qualif Add(v: number, n: number, m: number): v = m + n */

/// <reference path="mulThms.d.ts" />

module NavierStokes {
    /*@ solver :: {v:FluidField<Mutable> | offset(v,"width") = 128 && offset(v,"height") = 128} + null */
    var solver:FluidField = null;
    /*@ nsFrameCounter :: number */
    var nsFrameCounter = 0;

    export function runNavierStokes()
    {
        var solverRO /*@ readonly */ = solver;
        if (!solverRO) throw new Error("solver is null! did you forget to call setupNavierStokes first?");
        solverRO.update();
        nsFrameCounter++;

        if(nsFrameCounter===15)
            checkResult(solverRO.getDens());
    }

    /*@ checkResult :: ({v:IArray<number> | (len v) = (128+2) * (128+2)}) => void */
    function checkResult(dens) {
    
        var _lemma = mulThm130(130);
        var result = 0;
        for (var i=7000;i<7100;i++) {
            result+=~~((dens[i]*10));
        }

        if (result!==74) {
            console.log("checksum failed: " + result);
        }
    }

    export function setupNavierStokes()
    {
        var _lemma = mulThm128(128);
        var solverRO /*@ readonly */ = new FluidField(null, 128, 128);
        solverRO.setIterations(20);
        solverRO.setDisplayFunction(function(f:Field)
            /*@ <anonymous> (Field<Immutable>) => void */
            {});
        solverRO.setUICallback(prepareFrame);
        solverRO.reset();
        solver = solverRO;
    }

    export function tearDownNavierStokes()
    {
        solver = null;
    }

    /*@ addPoints :: ({v:Field<Mutable> | offset(v,"w") = 128 && offset(v,"h") = 128}) => void */
    function addPoints(field:Field) {
        var n = 64;
        for (var i = 1; i <= n; i++) {
            field.setVelocity(i, i, n, n);
            field.setDensity(i, i, 5);
            field.setVelocity(i, n - i, -n, -n);
            field.setDensity(i, n - i, 20);
            field.setVelocity(128 - i, n + i, -n, -n);
            field.setDensity(128 - i, n + i, 30);
        }
    }

    /*@ framesTillAddingPoints :: number */
    var framesTillAddingPoints = 0;
    /*@ framesBetweenAddingPoints :: number */
    var framesBetweenAddingPoints = 5;

    /*@ prepareFrame :: ({v:Field<Mutable> | offset(v,"w") = 128 && offset(v,"h") = 128}) => void */
    function prepareFrame(field:Field)
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
    export class FluidField {
        /*@ width : [Immutable] pos */
        private width;
        /*@ height : [Immutable] pos */
        private height;
        /*@ rowSize : [Immutable] {v:number | v = this.width + 2} */
        private rowSize;
        /*@ size : [Immutable] {v:number | v = (this.height+2) * (this.width+2)} */
        private size;
        /*@ dens :      [Immutable] {v:IArray<number> | (len v) = this.size} */
        private dens;
        /*@ dens_prev : [Immutable] {v:IArray<number> | (len v) = this.size} */
        private dens_prev;
        /*@ u :         [Immutable] {v:IArray<number> | (len v) = this.size} */
        private u;
        /*@ u_prev :    [Immutable] {v:IArray<number> | (len v) = this.size} */
        private u_prev;
        /*@ v :         [Immutable] {v:IArray<number> | (len v) = this.size} */
        private v;
        /*@ v_prev :    [Immutable] {v:IArray<number> | (len v) = this.size} */
        private v_prev;
        private iters:number;
        private visc:number;
        private dt:number;
        private displayFunc: (f:Field) => void;

        /*@ uiCallback : ({v:Field<Mutable> | offset(v,"w") = this.width && offset(v,"h") = this.height}) => void */
        private uiCallback = function(field:Field) 
            /*@ <anonymous> (Field<Mutable>)=>void */ 
            {};

        /*@ new (canvas:top, 
                 hRes:pos, 
                 wRes:{v:pos | hRes * v < 1000000}) => {v:FluidField<M> | offset(v,"width") = wRes && offset(v,"height") = hRes} */
        constructor(canvas, hRes, wRes) {
            var width = wRes;
            var height = hRes;
            var size = (height+2) * (width+2);

            this.width = width;
            this.height = height;
            this.rowSize = width + 2;
            this.size = size;
            var dens      = new Array<number>(size);
            var dens_prev = new Array<number>(size);
            var u         = new Array<number>(size);
            var u_prev    = new Array<number>(size);
            var v         = new Array<number>(size);
            var v_prev    = new Array<number>(size);
            for (var i = 0; i < size; i++) {
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

            this.displayFunc = function(f:Field) 
                /*@ <anonymous> (Field<Immutable>)=>void */ 
                {}; //ORIG: null
        }

            /*@ addFields : (x:{v:IArray<number> | (len v) = this.size}, 
                             s:{v:IArray<number> | (len v) = this.size}, 
                             dt:number) : void */
            addFields(x:number[], s:number[], dt:number)
            {
                for (var i=0; i<this.size; i++) x[i] += dt*s[i];
            }

            /*@ set_bnd : (b:number, x:{v:IArray<number> | (len v) = this.size}) : void */
            set_bnd(b:number, x:number[])
            {
                var width   = this.width;
                var height  = this.height;
                var rowSize = this.rowSize;

                var _lemma0 = mulThm1(rowSize, height+2);

                var _lemma1 = mulThm2(rowSize, height, height+2);
                var _lemma2 = mulThm2(rowSize, height+1, height+2);

                if (b===1) {
                    for (var i = 1; i <= width; i++) {
                        x[i] =  x[i + rowSize];
                        x[i + (height+1) *rowSize] = x[i + height * rowSize];
                    }
                    for (var j = 1; j <= height; j++) {
                        var _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] = -x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] = -x[width + j * rowSize];
                    }
                } else if (b === 2) {
                    for (var i = 1; i <= width; i++) {
                        x[i] = -x[i + rowSize];
                        x[i + (height + 1) * rowSize] = -x[i + height * rowSize];
                    }

                    for (var j = 1; j <= height; j++) {
                        var _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] =  x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] =  x[width + j * rowSize];
                    }
                } else {
                    for (var i = 1; i <= width; i++) {
                        x[i] =  x[i + rowSize];
                        x[i + (height + 1) * rowSize] = x[i + height * rowSize];
                    }

                    for (var j = 1; j <= height; j++) {
                        var _lemmaJ = mulThm2(rowSize, j, height+2);
                        x[j * rowSize] =  x[1 + j * rowSize];
                        x[(width + 1) + j * rowSize] =  x[width + j * rowSize];
                    }
                }
                var maxEdge = (height + 1) * rowSize;
                x[0]                 = 1/2 * (x[1] + x[rowSize]);//.
                x[maxEdge]           = 1/2 * (x[1 + maxEdge] + x[height * rowSize]);//.
                x[(width+1)]         = 1/2 * (x[width] + x[(width + 1) + rowSize]);//.
                x[(width+1)+maxEdge] = 1/2 * (x[width + maxEdge] + x[(width + 1) + height * rowSize]);//.
            }

            /*@ lin_solve : (b :number, 
                             x :{v:IArray<number> | (len v) = this.size}, 
                             x0:{v:IArray<number> | (len v) = this.size}, 
                             a :number, 
                             c :{v:number | v != 0}) : void */
            lin_solve(b:number, x:number[], x0:number[], a:number, c:number)
            {
                var width = this.width;
                var height = this.height;
                var rowSize = this.rowSize;

                if (a === 0 && c === 1) {
                    for (var j=1 ; j<=height; j++) {
                        var currentRow = j * rowSize;
                        var _lemma = mulThm2(rowSize, j, height+2);
                        ++currentRow;
                        for (var i = 0; i < width; i++) {
                            x[currentRow] = x0[currentRow];
                            ++currentRow;
                        }
                    }
                    this.set_bnd(b, x);
                } else {
                    var invC = 1 / c;
                    for (var k=0 ; k<this.iters; k++) {
                        for (var j=1 ; j<=height; j++) {
                            var lastRow = (j - 1) * rowSize;
                            var currentRow = j * rowSize;
                            var nextRow = (j + 1) * rowSize;
                            var _lemma1 = mulThm2(rowSize, j-1, height+2);
                            var _lemma2 = mulThm2(rowSize, j, height+2);
                            var _lemma3 = mulThm2(rowSize, j+1, height+2);
                            var lastX = x[currentRow];
                            ++currentRow;
                            for (var i=1; i<=width; i++) {
                                x[currentRow] = (x0[currentRow] + a*(lastX+x[++currentRow]+x[++lastRow]+x[++nextRow])) * invC;
                                lastX = x[currentRow];
                            }
                        }
                        this.set_bnd(b, x);
                    }
                }
            }

            /*@ diffuse : (b :number, 
                           x :{v:IArray<number> | (len v) = this.size}, 
                           x0:{v:IArray<number> | (len v) = this.size}, 
                           dt:number) : void */
            diffuse(b:number, x:number[], x0:number[], dt:number)
            {
                var a = 0;
                this.lin_solve(b, x, x0, a, 1 + 4*a);
            }

            /*@ lin_solve2 : (x :{v:IArray<number> | (len v) = this.size}, 
                              x0:{v:IArray<number> | (len v) = this.size}, 
                              y :{v:IArray<number> | (len v) = this.size}, 
                              y0:{v:IArray<number> | (len v) = this.size}, 
                              a :number, 
                              c :{v:number | v != 0}) : void */
            lin_solve2(x:number[], x0:number[], y:number[], y0:number[], a:number, c:number)
            {
                var width = this.width;
                var height = this.height;
                var rowSize = this.rowSize;

                if (a === 0 && c === 1) {
                    for (var j=1 ; j <= height; j++) {
                        var currentRow = j * rowSize;
                        var _lemma = mulThm2(rowSize, j, height+2);
                        ++currentRow;
                        for (var i = 0; i < width; i++) {
                            x[currentRow] = x0[currentRow];
                            y[currentRow] = y0[currentRow];
                            ++currentRow;
                        }
                    }
                    this.set_bnd(1, x);
                    this.set_bnd(2, y);
                } else {
                    var invC = 1/c;
                    for (var k=0 ; k<this.iters; k++) {
                        for (var j=1 ; j <= height; j++) {
                            var lastRow = (j - 1) * rowSize;
                            var currentRow = j * rowSize;
                            var nextRow = (j + 1) * rowSize;
                            var _lemma1 = mulThm2(rowSize, j-1, height+2);
                            var _lemma2 = mulThm2(rowSize, j, height+2);
                            var _lemma3 = mulThm2(rowSize, j+1, height+2);
                            var lastX = x[currentRow];
                            var lastY = y[currentRow];
                            ++currentRow;
                            for (var i = 1; i <= width; i++) {
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

            /*@ diffuse2 : (x :{v:IArray<number> | (len v) = this.size}, 
                            x0:{v:IArray<number> | (len v) = this.size}, 
                            y :{v:IArray<number> | (len v) = this.size}, 
                            y0:{v:IArray<number> | (len v) = this.size}, 
                            dt:number) : void */
            diffuse2(x:number[], x0:number[], y:number[], y0:number[], dt:number)
            {
                var a = 0;
                this.lin_solve2(x, x0, y, y0, a, 1 + 4 * a);
            }

            /*@ advect : (b :number, 
                          d :{v:IArray<number> | (len v) = this.size}, 
                          d0:{v:IArray<number> | (len v) = this.size}, 
                          u :{v:IArray<number> | (len v) = this.size}, 
                          v :{v:IArray<number> | (len v) = this.size}, 
                          dt:number) : void */
            advect(b:number, d:number[], d0:number[], u:number[], ww:number[], dt:number)
            {
                var width = this.width;
                var height = this.height;
                var rowSize = this.rowSize;

                var Wdt0 = dt * width;
                var Hdt0 = dt * height;
                var Wp5 = width + 1/2;//.
                var Hp5 = height + 1/2;//.
                for (var j = 1; j<= height; j++) {
                    var pos = j * rowSize;
                    var _lemma = mulThm2(rowSize, j, height+2);
                    for (var i = 1; i <= width; i++) {
                        var x:any = i - Wdt0 * u[++pos];
                        var y:any = j - Hdt0 * ww[pos]; // TODO revert 'ww' to 'v'
                        if (x < 1/2)//.
                            x = 1/2;//.
                        else if (x > Wp5)
                            x = Wp5;
                        var i0 = Math.floor(x); //ORIG: x | 0;
                        var i1 = i0 + 1;
                        if (y < 1/2)//.
                            y = 1/2;//.
                        else if (y > Hp5)
                            y = Hp5;
                        var j0 = Math.floor(y); //ORIG: y | 0;
                        var j1 = j0 + 1;
                        var s1 = x - i0;
                        var s0 = 1 - s1;
                        var t1 = y - j0;
                        var t0 = 1 - t1;
                        var row1 = j0 * rowSize;
                        var row2 = j1 * rowSize;
                        mulThm2(rowSize, j0, height+2);
                        mulThm2(rowSize, j1, height+2);
                        d[pos] = s0 * (t0 * d0[i0 + row1] + t1 * d0[i0 + row2]) + s1 * (t0 * d0[i1 + row1] + t1 * d0[i1 + row2]);
                    }
                }
                this.set_bnd(b, d);
            }

            /*@ project : (u  :{v:IArray<number> | (len v) = this.size}, 
                           v  :{v:IArray<number> | (len v) = this.size}, 
                           p  :{v:IArray<number> | (len v) = this.size}, 
                           div:{v:IArray<number> | (len v) = this.size}) : void */
            project(u:number[], ww:number[], p:number[], div:number[])
            {
                var width = this.width;
                var height = this.height;
                var rowSize = this.rowSize;

                var h = -(1/2) / Math.sqrt(width * height);//.
                for (var j = 1 ; j <= height; j++ ) {
                    var row = j * rowSize;
                    var previousRow = (j - 1) * rowSize;
                    var prevValue = row - 1;
                    var currentRow = row;
                    var nextValue = row + 1;
                    var nextRow = (j + 1) * rowSize;
                    var _lemma1 = mulThm2(rowSize, j-1, height+2);
                    var _lemma2 = mulThm2(rowSize, j, height+2);
                    var _lemma3 = mulThm2(rowSize, j+1, height+2);
                    for (var i = 1; i <= width; i++ ) {
                        div[++currentRow] = h * (u[++nextValue] - u[++prevValue] + ww[++nextRow] - ww[++previousRow]);
                        p[currentRow] = 0;
                    }
                }
                this.set_bnd(0, div);
                this.set_bnd(0, p);

                this.lin_solve(0, p, div, 1, 4 );
                var wScale = 1/2 * width;//.
                var hScale = 1/2 * height;//.
                for (var k = 1; k<= height; k++ ) {
                    var prevPos = k * rowSize - 1;
                    var currentPos = k * rowSize;
                    var nextPos = k * rowSize + 1;
                    var prevRow = (k - 1) * rowSize;
                    var currentRow = k * rowSize;
                    var nextRow = (k + 1) * rowSize;
                    var _lemma1 = mulThm2(rowSize, k-1, height+2);
                    var _lemma2 = mulThm2(rowSize, k, height+2);
                    var _lemma3 = mulThm2(rowSize, k+1, height+2);
                    for (var i = 1; i<= width; i++) {
                        ++currentPos;
                        u[currentPos] -= wScale * (p[++nextPos] - p[++prevPos]);
                        ww[currentPos]   -= hScale * (p[++nextRow] - p[++prevRow]);
                    }
                }
                this.set_bnd(1, u);
                this.set_bnd(2, ww);
            }

            /*@ dens_step : (x :{v:IArray<number> | (len v) = this.size}, 
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

            /*@ vel_step : (u :{v:IArray<number> | (len v) = this.size}, 
                            v :{v:IArray<number> | (len v) = this.size}, 
                            u0:{v:IArray<number> | (len v) = this.size}, 
                            v0:{v:IArray<number> | (len v) = this.size}, 
                            dt:number) : void */
            vel_step(u:number[], v:number[], u0:number[], v0:number[], dt:number)
            {
                this.addFields(u, u0, dt );
                this.addFields(v, v0, dt );
                var temp = u0; u0 = u; u = temp;
                temp = v0; v0 = v; v = temp;
                this.diffuse2(u,u0,v,v0, dt);
                this.project(u, v, u0, v0);
                temp = u0; u0 = u; u = temp;
                temp = v0; v0 = v; v = temp;
                this.advect(1, u, u0, u0, v0, dt);
                this.advect(2, v, v0, u0, v0, dt);
                this.project(u, v, u0, v0 );
            }

            /*@ queryUI : (d:{v:IArray<number> | (len v) = this.size}, 
                           u:{v:IArray<number> | (len v) = this.size}, 
                           v:{v:IArray<number> | (len v) = this.size}) : void */
            queryUI(d:number[], u:number[], v:number[])
            {
                for (var i = 0; i < this.size; i++) {
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
            /*@ setDisplayFunction : (this:FluidField<Mutable>, (Field<Immutable>)=>void) : void */
            public setDisplayFunction(f:(f:Field) => void) {
                this.displayFunc = f;
            }
            
            public iterations() { return this.iters; }
            /*@ setIterations : (this:FluidField<Mutable>, iters:number) : void */
            public setIterations(iters:number) 
            {
                if (iters > 0 && iters <= 100)
                    this.iters = iters;
            }
            /*@ setUICallback : (this:FluidField<Mutable>, 
                                 ({v:Field<Mutable> | offset(v,"w") = this.width && offset(v,"h") = this.height})=>void) : void */
            public setUICallback(callback:(f:Field) => void) {
                this.uiCallback = callback;
            }
            public reset()
            {
                for (var i = 0; i < this.size; i++) {
                    this.dens_prev[i] = 0; this.u_prev[i] = 0; this.v_prev[i] = 0; this.dens[i] = 0; this.u[i] = 0; this.v[i] = 0;
                }
            }
            /*@ getDens : () : {v:IArray<number> | (len v) = this.size} */
            public getDens()
            {
                return this.dens;
            }
    }

    export class Field {
        /*@ rowSize : [Immutable] {v:number | v = this.w + 2} */
        private rowSize;
        /*@ w : [Immutable] pos */
        private w;
        /*@ h : [Immutable] pos */
        private h;
        /*@ dens : [Immutable] {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private dens;
        /*@ u    : [Immutable] {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private u;
        /*@ v    : [Immutable] {v:IArray<number> | (len v) = (this.h + 2) * (this.w + 2)} */
        private v;

        /*@ new (rowSize: {v:number | v = w+2}, 
                 w:       pos, 
                 h:       pos, 
                 dens:    {v:IArray<number> | (len v) = (h+2) * (w+2)},
                 u:       {v:IArray<number> | (len v) = (h+2) * (w+2)},
                 v:       {v:IArray<number> | (len v) = (h+2) * (w+2)}) => {v:Field<M> | offset(v,"w") = w && offset(v,"h") = h } */
        constructor(rowSize:number, w:number, h:number, dens:number[], u:number[], v:number[]) {
            this.rowSize = rowSize;
            this.w = w;
            this.h = h;
            this.dens = dens;
            this.u = u;
            this.v = v;
        }

            /*@ setDensity : (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}, d:number) : void */
            public setDensity(x:number, y:number, d:number) {
                var _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                this.dens[(x + 1) + (y + 1) * this.rowSize] = d;
            }
            /*@ getDensity : (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getDensity(x:number, y:number) {
                var _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.dens[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ setVelocity : (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}, xv:number, yv:number) : void */
            public setVelocity(x:number, y:number, xv:number, yv:number) {
                var _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                this.u[(x + 1) + (y + 1) * this.rowSize] = xv;
                this.v[(x + 1) + (y + 1) * this.rowSize] = yv;
            }
            /*@ getXVelocity : (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getXVelocity(x:number, y:number) {
                var _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.u[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ getYVelocity : (x:{v:nat | v <= this.w}, y:{v:nat | v <= this.h}) : number */
            public getYVelocity(x:number, y:number) {
                var _lemma = mulThm2(this.rowSize, y+1, this.h+2);
                return this.v[(x + 1) + (y + 1) * this.rowSize];
            }
            /*@ width : () : {v:number | v = this.w} */
            public width():number { return this.w; }
            /*@ height : () : {v:number | v = this.h} */
            public height():number { return this.h; }
    }
}
