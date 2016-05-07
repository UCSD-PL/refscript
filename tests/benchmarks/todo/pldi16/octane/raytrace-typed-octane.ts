// The ray tracer code in this file is written by Adam Burmister. It
// is available in its original form from:
//
//   http://labs.nz.co/raytracer/
//
// It has been modified slightly by Google to work as a standalone
// benchmark, but the all the computational code remains
// untouched. This file also contains a copy of parts of the Prototype
// JavaScript framework which is used by the ray tracer.

//let RayTrace = new BenchmarkSuite('RayTrace', 739989, [
//  new Benchmark('RayTrace', renderScene)
//]);

// Variable used to hold a number that can be used to verify that
// the scene was ray traced correctly.

//TODO: move this stuff to prelude?
/*@ qualif HasP (x: string, y: A): hasProperty(x, y) */
/*@ qualif EnumP(x: string, y: A): enumProp(x, y) */

interface HTMLCanvasElement<M extends ReadOnly> {
    /*@ getContext : (string) => {CanvasRenderingContext2D<Mutable> | 0 < 1} */
    getContext(s:string):CanvasRenderingContext2D<Mutable>; //or WebGLRenderingContext or null
}
interface CanvasRenderingContext2D<M extends ReadOnly> {
    /*@ fillStyle : string */
    fillStyle:string;
    fillRect(a:number, b:number, c:number, d:number):void;
}

/*@ type EngineOptions = (Mutable) {
    canvasHeight: number;
    canvasWidth: number;
    pixelWidth: {number | v > 0};
    pixelHeight: {number | v > 0};
    renderDiffuse: boolean;
    renderShadows: boolean;
    renderHighlights: boolean;
    renderReflections: boolean;
    rayDepth: number
} */

//TODO: field initializers not actually doing anything, since constructors require all arguments
//TODO: had to add explicit toString calls in toStrings
//TODO: many classes allowed some members to be null; where usage made this seem inappropriate I removed that feature
module VERSION {

    export module RayTracer {

        declare type MVector = Vector<Mutable>
        declare type MColor = Color<Mutable>
        declare type IColor = Color<Immutable>
        declare type MIntersectionInfo = IntersectionInfo<Mutable>

        /*@ checkNumber :: number */
        let checkNumber:number=0;
        export class Color<M extends ReadOnly> {
            /*@ red : number */
            public red=0;
            /*@ green : number */
            public green=0;
            /*@ blue : number */
            public blue=0;

            /*@ new (red:number, green:number, blue:number) : {Color<M> | 0 < 1} */
            constructor(red?, green?, blue?) {
                this.red = red;
                this.green = green;
                this.blue = blue;
            }

            /*@ add <M1 extends ReadOnly, M2 extends ReadOnly> (c1:Color<M1>, c2:Color<M2>) : {MColor | 0 < 1} */
            public static add(c1, c2) {
                let result = new Color(0, 0, 0);

                result.red = c1.red + c2.red;
                result.green = c1.green + c2.green;
                result.blue = c1.blue + c2.blue;

                return result;
            }

            /*@ addScalar <M extends ReadOnly> (c1:Color<M>, s:number) : {MColor | 0 < 1} */
            public static addScalar(c1, s:number) {
                let result = new Color(0, 0, 0);

                result.red = c1.red + s;
                result.green = c1.green + s;
                result.blue = c1.blue + s;

                result.limit();

                return result;
            }


            /*@ subtract <M1 extends ReadOnly, M2 extends ReadOnly> (c1:Color<M1>, c2:Color<M2>) : {MColor | 0 < 1} */
            public static subtract(c1, c2) {
                let result = new Color(0, 0, 0);

                result.red = c1.red - c2.red;
                result.green = c1.green - c2.green;
                result.blue = c1.blue - c2.blue;

                return result;
            }

            /*@ multiply <M1 extends ReadOnly, M2 extends ReadOnly> (c1:Color<M1>, c2:Color<M2>) : {MColor | 0 < 1} */
            public static multiply(c1, c2) {
                let result = new Color(0, 0, 0);

                result.red = c1.red * c2.red;
                result.green = c1.green * c2.green;
                result.blue = c1.blue * c2.blue;

                return result;
            }

            /*@ multiplyScalar <M extends ReadOnly> (c1:Color<M>, f:number) : {MColor | 0 < 1} */
            public static multiplyScalar(c1, f:number) {
                let result = new Color(0, 0, 0);

                result.red = c1.red * f;
                result.green = c1.green * f;
                result.blue = c1.blue * f;

                return result;
            }


            /*@ divideFactor <M extends ReadOnly> (c1:Color<M>, f:{number | v != 0}) : {MColor | 0 < 1} */
            public static divideFactor(c1, f:number) {
                let result = new Color(0, 0, 0);

                result.red = c1.red / f;
                result.green = c1.green / f;
                result.blue = c1.blue / f;

                return result;
            }

            /*@ @Mutable limit () : {void | 0 < 1} */
            public limit() {
                this.red = (this.red > 0) ? ((this.red > 1) ? 1 : this.red) : 0;
                this.green = (this.green > 0) ? ((this.green > 1) ? 1 : this.green) : 0;
                this.blue = (this.blue > 0) ? ((this.blue > 1) ? 1 : this.blue) : 0;
            }

            /*@ distance <M extends ReadOnly> (color:Color<M>) : {number | 0 < 1} */
            public distance(color) {
                let d = Math.abs(this.red - color.red) + Math.abs(this.green - color.green) + Math.abs(this.blue - color.blue);
                return d;
            }

            /*@ blend <M1 extends ReadOnly, M2 extends ReadOnly> (c1:Color<M1>, c2:Color<M2>, w:number) : {MColor | 0 < 1} */
            public static blend(c1, c2, w:number) {
                let result = new Color(0, 0, 0);
                result = Color.add(
                    Color.multiplyScalar(c1, 1 - w),
                    Color.multiplyScalar(c2, w)
                );
                return result;
            }

            public brightness() {
                let r = Math.floor(this.red * 255);
                let g = Math.floor(this.green * 255);
                let b = Math.floor(this.blue * 255);
                return (r * 77 + g * 150 + b * 29) / 256 //ORIG: >> 8;
            }

            public toString() {
                let r = Math.floor(this.red * 255);
                let g = Math.floor(this.green * 255);
                let b = Math.floor(this.blue * 255);

                return "rgb(" + r + "," + g + "," + b + ")";
            }
        }

        export class Light<M extends ReadOnly> {
            /*@ position : MVector */
            public position:MVector;
            /*@ color : IColor */
            public color:IColor;
            public intensity:number=10;

            /*@ new (position:MVector, color:IColor, intensity:number) : {Light<M> | 0 < 1} */
            constructor(position:MVector, color:IColor, intensity?) {
                this.position = position;
                this.color = color;
                this.intensity = intensity;
            }

            public toString() {
                return 'Light [' + this.position.x + ',' + this.position.y + ',' + this.position.z + ']';
            }
        }

        export class Vector<M extends ReadOnly> {
            /*@ x : number */
            public x = 0;
            /*@ y : number */
            public y = 0;
            /*@ z : number */
            public z = 0;

            /*@ new (x:number, y:number, z:number) : {Vector<M> | 0 < 1} */
            constructor(x?, y?, z?) {
                this.x = x;
                this.y = y;
                this.z = z;
            }

            /*@ @Mutable copy <M extends ReadOnly> (vector:Vector<M>) : {void | 0 < 1} */
            public copy(vector) {
                this.x = vector.x;
                this.y = vector.y;
                this.z = vector.z;
            }

            /*@ normalize () : Vector<Unique> */
            public normalize() {
                let m = this.magnitude();
                if (m === 0) throw new Error("Cannot normalize the 0-vector!");
                return new Vector(this.x / m, this.y / m, this.z / m);
            }

            public magnitude() {
                let x = this.x;
                let y = this.y;
                let z = this.z;
                return Math.sqrt((x * x) + (y * y) + (z * z));
            }

            /*@ cross <M extends ReadOnly> (w:Vector<M>) : {Vector<Unique> | 0 < 1} */
            public cross(w) {
                return new Vector(
                        -this.z * w.y + this.y * w.z,
                    this.z * w.x - this.x * w.z,
                        -this.y * w.x + this.x * w.y);
            }

            /*@ dot <M extends ReadOnly> (w:Vector<M>) : {number | 0 < 1} */
            public dot(w) {
                return this.x * w.x + this.y * w.y + this.z * w.z;
            }

            /*@ add <M1 extends ReadOnly, M2 extends ReadOnly> (v:Vector<M1>, w:Vector<M2>) : {Vector<Unique> | 0 < 1} */
            public static add(v, w) {
                return new Vector(w.x + v.x, w.y + v.y, w.z + v.z);
            }

            /*@ subtract <M1 extends ReadOnly, M2 extends ReadOnly> (v:Vector<M1>, w:Vector<M2>) : {Vector<Unique> | 0 < 1} */
            public static subtract(v, w) {
                if (!w || !v) throw 'Vectors must be defined [' + v + ',' + w + ']';
                return new Vector(v.x - w.x, v.y - w.y, v.z - w.z);
            }

            /*@ multiplyVector <M1 extends ReadOnly, M2 extends ReadOnly> (v:Vector<M1>, w:Vector<M2>) : {Vector<Unique> | 0 < 1} */
            public static multiplyVector(v, w) {
                return new Vector(v.x * w.x, v.y * w.y, v.z * w.z);
            }

            /*@ multiplyScalar <M extends ReadOnly> (v:Vector<M>, w:number) : {Vector<Unique> | 0 < 1} */
            public static multiplyScalar(v, w:number) {
                return new Vector(v.x * w, v.y * w, v.z * w);
            }

            public toString() {
                return 'Vector [' + this.x + ',' + this.y + ',' + this.z + ']';
            }
        }

        export class Ray<M extends ReadOnly> {
            /*@ position : MVector */
            public position:MVector;
            /*@ direction : MVector */
            public direction:MVector;

            /*@ new (position:MVector, direction:MVector) : {Ray<M> | 0 < 1} */
            constructor(position:MVector, direction:MVector) {
                this.position = position;
                this.direction = direction;
            }

            public toString() {
                return 'Ray [' + this.position.toString() + ',' + this.direction.toString() + ']';
            }
        }

        export class Scene<M extends ReadOnly> {
            public camera : Camera<Immutable>;
            /*@ shapes : IArray<Shape<Immutable>> */
            public shapes;
            /*@ lights : IArray<Light<Immutable>> */
            public lights;
            public background : Background<Immutable>;

            constructor() {
                this.camera = new Camera(
                    new Vector(0, 0, -5),
                    new Vector(0, 0, 1),
                    new Vector(0, 1, 0)
                );
                this.shapes = new Array<Shape<Immutable>>(0);
                this.lights = new Array<Light<Immutable>>(0);
                this.background = new Background(new Color(0, 0, 1/2), 1/5);
            }
        }

        // module Material {

        export class BaseMaterial<M extends ReadOnly> {
            public gloss:number = 2;
            public transparency:number = 0;
            public reflection:number = 0;
            public refraction:number = 1/2;
            public hasTexture:boolean = false;

            /*@ new (gloss:number, transparency:number, reflection:number, refraction:number, hasTexture:boolean) : {BaseMaterial<M> | 0 < 1} */
            constructor(gloss?,             // [0...infinity] 0 = matt
                        transparency?,      // 0=opaque
                        reflection?,       // [0...infinity] 0 = no reflection
                        refraction?,
                        hasTexture?) {
                this.gloss = gloss;
                this.transparency = transparency;
                this.reflection = reflection;
                this.refraction = refraction;
                this.hasTexture = hasTexture;
            }

            /*@ getColor (u:number, v:number) : {IColor | 0 < 1} */
            public getColor(u:number, v:number) : IColor {
                throw "Abstract method";
            }

            /*@ wrapUp (t:number) : {number | 0 < 1} */
            public wrapUp(t:number) {
                t = t % 2;
                if (t < -1) t = t + 2 //ORIG: t += 2;
                if (t >= 1) t = t - 2 //ORIG: t -= 2;
                return t;
            }

            public toString() {
                return 'Material [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Solid<M extends ReadOnly> extends BaseMaterial<M> {
            public color:IColor;

            /*@ new (color:IColor, reflection:number, refraction:number, transparency:number, gloss:number) : {Solid<M> | 0 < 1} */
            constructor(color:IColor, reflection:number, refraction:number, transparency:number, gloss:number) {
                super(gloss, transparency, reflection, refraction, false);
                this.color = color;
            }

            /*@ getColor (u:number, v:number) : {IColor | 0 < 1} */
            public getColor(u:number, v:number) : IColor {
                return this.color;
            }

            public toString() {
                return 'SolidMaterial [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Chessboard<M extends ReadOnly> extends BaseMaterial<M> {
            public colorEven:IColor;
            public colorOdd:IColor;
            public density:number = 1/2;

            /*@ new (colorEven:IColor, colorOdd:IColor, reflection:number, transparency:number, gloss:number, density:number) : {Chessboard<M> | 0 < 1} */
            constructor(colorEven:IColor, colorOdd:IColor,
                        reflection:number,
                        transparency:number,
                        gloss:number,
                        density?) {
                super(gloss, transparency, reflection, 1/2, true);
                this.colorEven = colorEven;
                this.colorOdd = colorOdd;
                this.density = density;
            }

            /*@ getColor (u:number, v:number) : {IColor | 0 < 1} */
            public getColor(u:number, v:number) : IColor {
                let t = this.wrapUp(u * this.density) * this.wrapUp(v * this.density);

                if (t < 0)
                    return this.colorEven;
                else
                    return this.colorOdd;
            }

            public toString() {
                return 'ChessMaterial [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Shape<M extends ReadOnly> {
            /*@ position : MVector */
            public position:MVector;
            public material:BaseMaterial<Immutable>;

            /*@ new (position:MVector, material:BaseMaterial<Immutable>) : {Shape<M> | 0 < 1} */
            constructor(position:MVector, material) {
                this.position = position;
                this.material = material;
            }

            /*@ intersect <M extends ReadOnly> (ray:Ray<M>) : {MIntersectionInfo | 0 < 1} */
            public intersect(ray) : MIntersectionInfo {
                throw "Abstract method";
            }
        }

        export class Sphere<M extends ReadOnly> extends Shape<M> {
            public radius:number;

            /*@ new (position:MVector, radius:number, material:BaseMaterial<Immutable>) : {Sphere<M> | 0 < 1} */
            constructor(position:MVector, radius:number, material) {
                super(position, material);
                this.radius = radius;
            }

            /*@ intersect <M extends ReadOnly> (ray:Ray<M>) : {MIntersectionInfo | 0 < 1} */
            public intersect(ray) : MIntersectionInfo {
                let info = new IntersectionInfo(false, 0, null, null, null, null, null);
                info.shape = <Shape<ReadOnly>>this;

                let dst:MVector = Vector.subtract(ray.position, this.position);

                let B = dst.dot(ray.direction);
                let C = dst.dot(dst) - (this.radius * this.radius);
                let D = (B * B) - C;

                if (D > 0) { // intersection!
                    info.isHit = true;
                    let infoDist = (-B) - Math.sqrt(D);
                    info.distance = infoDist;
                    let infoPos:MVector = Vector.add(
                        ray.position,
                        Vector.multiplyScalar(
                            ray.direction,
                            infoDist
                        )
                    );
                    info.position = infoPos;
                    info.normal = Vector.subtract(
                        infoPos,
                        this.position
                    ).normalize();

                    info.color = this.material.getColor(0, 0);
                } else {
                    info.isHit = false;
                }
                return info;
            }

            public toString() {
                return 'Sphere [position=' + this.position.toString() + ', radius=' + this.radius + ']';
            }
        }

        export class Plane<M extends ReadOnly> extends Shape<M> {
            public d:number;

            /*@ new (position:MVector, d:number, material:BaseMaterial<Immutable>) : {Plane<M> | 0 < 1} */
            constructor(position:MVector, d:number, material) {
                super(position, material);
                this.d = d;
            }

            /*@ intersect <M extends ReadOnly> (ray:Ray<M>) : {MIntersectionInfo | 0 < 1} */
            public intersect(ray) : MIntersectionInfo {
                let info = new IntersectionInfo(false, 0, null, null, null, null, null);

                let Vd = this.position.dot(ray.direction);
                if (Vd === 0) return info; // no intersection

                let t = -(this.position.dot(ray.position) + this.d) / Vd;
                if (t <= 0) return info;

                info.shape = <Shape<ReadOnly>>this;
                info.isHit = true;
                let infoPos:MVector = Vector.add(
                    ray.position,
                    Vector.multiplyScalar(
                        ray.direction,
                        t
                    )
                );
                info.position = infoPos;
                info.normal = this.position;
                info.distance = t;

                if (this.material.hasTexture) {
                    let vU:MVector = new Vector(this.position.y, this.position.z, -this.position.x);
                    let vV:MVector = vU.cross(this.position);
                    let u = infoPos.dot(vU);
                    let v = infoPos.dot(vV);
                    info.color = this.material.getColor(u, v);
                } else {
                    info.color = this.material.getColor(0, 0);
                }

                return info;
            }

            public toString() {
                return 'Plane [' + this.position.toString() + ', d=' + this.d + ']';
            }
        }
        // }

        export class IntersectionInfo<M extends ReadOnly> {
            /*@ isHit : boolean */
            public isHit = false;
            /*@ hitCount : number */
            public hitCount = 0;
            /*@ shape : Shape<ReadOnly> + null */
            public shape = null;
            /*@ position : MVector + null */
            public position = null;
            /*@ normal : MVector + null */
            public normal = null;
            /*@ color : IColor + null */
            public color = null;
            /*@ distance : number + null */
            public distance = null;

            /*@ new (isHit:boolean,
                    hitCount:number,
                    shape:Shape<Immutable> + null,
                    position:MVector + null,
                    normal:MVector + null,
                    color:IColor + null,
                    distance:number + null) : {IntersectionInfo<M> | 0 < 1} */
            constructor(isHit?, hitCount?, shape?, position?, normal?, color?, distance?) {
                this.isHit = isHit;
                this.hitCount = hitCount;
                this.shape = shape;
                this.position = position;
                this.normal = normal;
                this.color = color;
                this.distance = distance;
            }

            /*@ @Mutable initialize () : {void | 0 < 1} */
            public initialize() {
                this.color = <IColor>(new Color(0, 0, 0));
            }

            public toString() {
                let position = this.position;
                if (!position) return 'Intersection [position==null]';
                return 'Intersection [' + position.toString() + ']';
            }
        }

        export class Camera<M extends ReadOnly> {
            public equator:MVector;
            public screen:MVector;

            /*@ position : MVector */
            public position:MVector;
            /*@ lookAt : MVector */
            public lookAt:MVector;
            /*@ up : MVector */
            public up:MVector;

            /*@ new (position:MVector, lookAt:MVector, up:MVector) : {Camera<M> | 0 < 1} */
            constructor(position:MVector,
                        lookAt:MVector,
                        up:MVector) {
                this.equator = lookAt.normalize().cross(up);
                this.screen = Vector.add(position, lookAt);
                this.position = position;
                this.lookAt = lookAt;
                this.up = up;
            }

            /*@ getRay (vx:number, vy:number) : {Ray<Unique> | 0 < 1} */
            public getRay(vx:number, vy:number) {
                let pos:MVector = Vector.subtract(
                    this.screen,
                    Vector.subtract(
                        Vector.multiplyScalar(this.equator, vx),
                        Vector.multiplyScalar(this.up, vy)
                    )
                );
                pos.y = pos.y * -1;
                let dir = Vector.subtract(
                    pos,
                    this.position
                );

                let ray = new Ray(pos, dir.normalize());

                return ray;
            }

            public toString() {
                return 'Ray []';
            }
        }

        export class Background<M extends ReadOnly> {
            /*@ color : MColor */
            public color:MColor;
            public ambience:number = 0;

            /*@ new (color:MColor, ambience:number) : {Background<M> | 0 < 1} */
            constructor(color, ambience?) {
                this.color = color;
                this.ambience = ambience;
            }
        }

        /*@ extend :: (dest:(Mutable){[s:string]:top}, src:(Immutable){[s:string]:top}) => {(Mutable){[s:string]:top} | 0 < 1} */
        function extend(dest, src) {
            // PV TODO
            for (let p in src) {
                dest[p] = src[p];
            }
            return dest;
        }

        export class Engine<M extends ReadOnly> {
            /*@ canvas : CanvasRenderingContext2D<Mutable> + null */
            public canvas:CanvasRenderingContext2D<Mutable> = null; /* 2d context we can render to */
            /*@ options : EngineOptions */
            public options;

            /*@ new (options:EngineOptions) : {Engine<M> | 0 < 1} */
            constructor(options) {
                // ORIG:
                // let this_options = extend({
                //     canvasHeight: 100,
                //     canvasWidth: 100,
                //     pixelWidth: 2,
                //     pixelHeight: 2,
                //     renderDiffuse: false,
                //     renderShadows: false,
                //     renderHighlights: false,
                //     renderReflections: false,
                //     rayDepth: 2
                // }, options || {});
                let this_options = options;

                this_options.canvasHeight = this_options.canvasHeight / this_options.pixelHeight; //ORIG: /=
                this_options.canvasWidth = this_options.canvasWidth / this_options.pixelWidth; //ORIG: /=

                this.options = this_options;

                /* TODO: dynamically include other scripts */
            }

            /*@ setPixel <M extends ReadOnly>(x:number, y:number, color:Color<M>) : {void | 0 < 1} */
            public setPixel(x, y, color) {
                let pxW = this.options.pixelWidth;
                let pxH = this.options.pixelHeight;

                let canvas = this.canvas;
                if (canvas) {
                    (<CanvasRenderingContext2D<Mutable>>canvas).fillStyle = color.toString();
                    canvas.fillRect(x * pxW, y * pxH, pxW, pxH);
                } else {
                    if (x === y) {
                        checkNumber += color.brightness();
                    }
                    // print(x * pxW, y * pxH, pxW, pxH);
                }
            }

            /*@ @Mutable renderScene <M1 extends ReadOnly, M2 extends ReadOnly> (scene:Scene<M1>, canvas:HTMLCanvasElement<M2> + undefined) : {void | 0 < 1} */
            public renderScene(scene, canvas) {
                checkNumber = 0;
                /* Get canvas */
                if (canvas) {
                    this.canvas = canvas.getContext("2d");
                } else {
                    this.canvas = null;
                }

                let canvasHeight = this.options.canvasHeight;
                let canvasWidth = this.options.canvasWidth;

                for (let y = 0; y < canvasHeight; y++) {
                    for (let x = 0; x < canvasWidth; x++) {
                        let yp = y * 1 / canvasHeight * 2 - 1;
                        let xp = x * 1 / canvasWidth * 2 - 1;

                        let ray:Ray<Mutable> = scene.camera.getRay(xp, yp);

                        let color = this.getPixelColor(ray, scene);

                        this.setPixel(x, y, color);
                    }
                }
                if (checkNumber !== 2321) {
                    throw new Error("Scene rendered incorrectly");
                }
            }

            /*@ getPixelColor <M1 extends ReadOnly, M2 extends ReadOnly> (ray:Ray<M1>, scene:Scene<M2>) : {MColor | 0 < 1} */
            public getPixelColor(ray, scene) {
                let info = this.testIntersection(ray, scene, undefined);
                if (info.isHit) {
                    let color = this.rayTrace(info, ray, scene, 0);
                    return color;
                }
                return scene.background.color;
            }

            /*@ testIntersection <M1 extends ReadOnly, M2 extends ReadOnly, M3 extends ReadOnly> (ray:Ray<M1>, scene:Scene<M2>, exclude:Shape<M3> + undefined) : {MIntersectionInfo | 0 < 1} */
            public testIntersection(ray, scene, exclude?) {
                let hits = 0;
                /*@ best :: MIntersectionInfo */
                let best = new IntersectionInfo(false, 0, null, null, null, null, null);
                best.distance = 2000;

                let sceneShapes = scene.shapes;
                for (let i = 0; i < sceneShapes.length; i++) {
                    let shape = sceneShapes[i];

                    if (shape !== exclude) {
                        let info = shape.intersect(ray);
                        if (info.isHit && info.distance >= 0 && info.distance < best.distance) {
                            best = info;
                            hits++;
                        }
                    }
                }
                best.hitCount = hits;
                return best;
            }

            /*@ getReflectionRay <M1 extends ReadOnly, M2 extends ReadOnly> (P:MVector, N:Vector<M1>, V:Vector<M2>) : {Ray<M> | 0 < 1} */
            public getReflectionRay(P, N, V) {
                let c1 = -N.dot(V);
                let R1 = Vector.add(
                    Vector.multiplyScalar(N, 2 * c1),
                    V
                );
                return new Ray(P, R1);
            }

            /*@ rayTrace <M1 extends ReadOnly, M2 extends ReadOnly, M3 extends ReadOnly> (info:IntersectionInfo<M1>, ray:Ray<M2>, scene:Scene<M3>, depth:number) : {MColor | 0 < 1} */
            public rayTrace(info, ray, scene, depth) {
                let infoColor = info.color;
                let infoShape = info.shape;
                let infoNormal = info.normal;
                let infoPosition = info.position;

                // if (!infoColor ||
                //     !infoShape ||
                //     !infoNormal ||
                //     !infoPosition) throw new Error('incomplete IntersectionInfo'); //TODO is there a way to get rid of this check?
                if (!infoColor)    throw new Error('incomplete IntersectionInfo'); //TODO can we at least get the more compact version above?
                if (!infoShape)    throw new Error('incomplete IntersectionInfo');
                if (!infoNormal)   throw new Error('incomplete IntersectionInfo');
                if (!infoPosition) throw new Error('incomplete IntersectionInfo');

                // Calc ambient
                let color = Color.multiplyScalar(infoColor, scene.background.ambience);
                let oldColor = color;
                let shininess = Math.pow(10, infoShape.material.gloss + 1);

                let sceneLights = scene.lights;
                for (let i = 0; i < sceneLights.length; i++) {
                    let light = sceneLights[i];

                    // Calc diffuse lighting
                    let v:MVector = Vector.subtract(
                        light.position,
                        infoPosition
                    ).normalize();

                    if (this.options.renderDiffuse) {
                        let L = v.dot(infoNormal);
                        if (L > 0) {
                            color = Color.add(
                                color,
                                Color.multiply(
                                    infoColor,
                                    Color.multiplyScalar(
                                        light.color,
                                        L
                                    )
                                )
                            );
                        }
                    }

                    // The greater the depth the more accurate the colours, but
                    // this is exponentially (!) expensive
                    if (depth <= this.options.rayDepth) {
                        // calculate reflection ray
                        if (this.options.renderReflections && infoShape.material.reflection > 0) {
                            let reflectionRay = this.getReflectionRay(infoPosition, infoNormal, ray.direction);
                            let refl = this.testIntersection(reflectionRay, scene, infoShape);

                            let reflColor;
                            if (refl.isHit && refl.distance > 0) {
                                reflColor = this.rayTrace(refl, reflectionRay, scene, depth + 1);
                            } else {
                                reflColor = scene.background.color;
                            }

                            color = Color.blend(
                                color,
                                reflColor,
                                infoShape.material.reflection
                            );
                        }

                        // Refraction
                        /* TODO */
                    }

                    /* Render shadows and highlights */

                    let shadowInfo = new IntersectionInfo(false, 0, null, null, null, null, null);

                    if (this.options.renderShadows) {
                        let shadowRay:Ray<ReadOnly> = new Ray(infoPosition, v);

                        shadowInfo = this.testIntersection(shadowRay, scene, infoShape);
                        if (shadowInfo.isHit && shadowInfo.shape !== infoShape /*&& shadowInfo.shape.type != 'PLANE'*/) {
                            let vA = Color.multiplyScalar(color, 1/2);
                            let shadowInfoShape = shadowInfo.shape;
                            if (!shadowInfoShape) throw new Error('This should probably never happen');
                            let dB = (1/2 * Math.pow(shadowInfoShape.material.transparency, 1/2));
                            color = Color.addScalar(vA, dB);
                        }
                    }

                    // Phong specular highlights
                    if (this.options.renderHighlights && !shadowInfo.isHit && infoShape.material.gloss > 0) {
                        let Lv = Vector.subtract(
                            infoShape.position,
                            light.position
                        ).normalize();

                        let E = Vector.subtract(
                            scene.camera.position,
                            infoShape.position
                        ).normalize();

                        let H:MVector = Vector.subtract(
                            E,
                            Lv
                        ).normalize();

                        let glossWeight = Math.pow(Math.max(infoNormal.dot(H), 0), shininess);
                        color = Color.add(
                            Color.multiplyScalar(light.color, glossWeight),
                            color
                        );
                    }
                }
                color.limit();
                return color;
            }
        }
        // }

        export function renderScene() {
            let scene:Scene<Mutable> = new Scene();

            scene.camera = new Camera(
                new Vector(0, 0, -15),
                new Vector(-1/5, 0, 5),
                new Vector(0, 1, 0)
            );

            scene.background = new Background(
                new Color(1/2, 1/2, 1/2),
                2/5
            );

            let sphere:Shape<Immutable> = new Sphere(
                new Vector(-3/2, 3/2, 2),
                3/2,
                new Solid(
                    new Color(0, 1/2, 1/2),
                    3/10,
                    0,
                    0,
                    2
                )
            );

            let sphere1:Shape<Immutable> = new Sphere(
                new Vector(1, 1/4, 1),
                1/2,
                new Solid(
                    new Color(9/10, 9/10, 9/10),
                    1/10,
                    0,
                    0,
                    3/2
                )
            );

            let plane:Shape<Immutable> = new Plane(
                new Vector(1/10, 9/10, -1/2).normalize(),
                6/5,
                new Chessboard(
                    new Color(1, 1, 1),
                    new Color(0, 0, 0),
                    1/5,
                    0,
                    1,
                    7/10
                )
            );

            // ORIG:
            // scene.shapes.push(plane);
            // scene.shapes.push(sphere);
            // scene.shapes.push(sphere1);
            scene.shapes = [plane, sphere, sphere1];

            let light:Light<Immutable> = new Light(
                new Vector(5, 10, -1),
                new Color(4/5, 4/5, 4/5),
                10 // (ORIG: default param omitted)
            );

            let light1:Light<Immutable> = new Light(
                new Vector(-3, 5, -15),
                new Color(4/5, 4/5, 4/5),
                100
            );

            // ORIG:
            // scene.lights.push(light);
            // scene.lights.push(light1);
            scene.lights = [light, light1];

            let imageWidth = 100; // $F('imageWidth');
            let imageHeight = 100; // $F('imageHeight');
            /*@ pixelSize :: {IArray<{number | v > 0}> | len(v) = 2} */
            let pixelSize = [5,5];//"5,5".split(','); //  $F('pixelSize').split(',');
            let renderDiffuse = true; // $F('renderDiffuse');
            let renderShadows = true; // $F('renderShadows');
            let renderHighlights = true; // $F('renderHighlights');
            let renderReflections = true; // $F('renderReflections');
            let rayDepth = 2;//$F('rayDepth');

            let raytracer = new Engine(
                {
                    canvasWidth: imageWidth,
                    canvasHeight: imageHeight,
                    pixelWidth: pixelSize[0],
                    pixelHeight: pixelSize[1],
                    renderDiffuse: renderDiffuse,
                    renderHighlights: renderHighlights,
                    renderShadows: renderShadows,
                    renderReflections: renderReflections,
                    rayDepth: rayDepth
                }
            );
            raytracer.renderScene(scene, undefined);
        }

    }
}
