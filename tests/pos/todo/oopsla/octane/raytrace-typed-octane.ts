// The ray tracer code in this file is written by Adam Burmister. It
// is available in its original form from:
//
//   http://labs.nz.co/raytracer/
//
// It has been modified slightly by Google to work as a standalone
// benchmark, but the all the computational code remains
// untouched. This file also contains a copy of parts of the Prototype
// JavaScript framework which is used by the ray tracer.

//var RayTrace = new BenchmarkSuite('RayTrace', 739989, [
//  new Benchmark('RayTrace', renderScene)
//]);

// Variable used to hold a number that can be used to verify that
// the scene was ray traced correctly.

/*@ alias MVector = Vector<Mutable> */
/*@ alias MColor = Color<Mutable> */
/*@ alias IColor = Color<Immutable> */
/*@ alias MIntersectionInfo = IntersectionInfo<Mutable> */

//TODO: move this stuff to prelude?
/*@ qualif Bot(s:Str,v:a): hasProperty(s,v) */
/*@ qualif Bot(s:Str,v:a): enumProp(s,v) */
interface HTMLCanvasElement {
    /*@ getContext : (string) => {CanvasRenderingContext2D<Mutable> | true} */
    getContext(s:string):CanvasRenderingContext2D; //or WebGLRenderingContext or null
}
interface CanvasRenderingContext2D {
    /*@ fillStyle : string */
    fillStyle:string;
    fillRect(a:number, b:number, c:number, d:number):void;
}

/*@ alias EngineOptions = [Mutable] {
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
        /*@ checkNumber :: number */
        var checkNumber:number=0;
        export class Color {
            /*@ red : number */
            public red=0;
            /*@ green : number */
            public green=0;
            /*@ blue : number */
            public blue=0;

            /*@ new(red:number, green:number, blue:number) => {Color<M> | true} */
            constructor(red?, green?, blue?) {
                this.red = red;
                this.green = green;
                this.blue = blue;
            }

            /*@ add : forall M1 M2 . (c1:Color<M1>, c2:Color<M2>) : {MColor | true} */
            public static add(c1:Color, c2:Color) {
                var result = new Color(0, 0, 0);

                result.red = c1.red + c2.red;
                result.green = c1.green + c2.green;
                result.blue = c1.blue + c2.blue;

                return result;
            }

            /*@ addScalar : forall M . (c1:Color<M>, s:number) : {MColor | true} */
            public static addScalar(c1:Color, s:number) {
                var result = new Color(0, 0, 0);

                result.red = c1.red + s;
                result.green = c1.green + s;
                result.blue = c1.blue + s;

                result.limit();

                return result;
            }


            /*@ subtract : forall M1 M2 . (c1:Color<M1>, c2:Color<M2>) : {MColor | true} */
            public static subtract(c1:Color, c2:Color) {
                var result = new Color(0, 0, 0);

                result.red = c1.red - c2.red;
                result.green = c1.green - c2.green;
                result.blue = c1.blue - c2.blue;

                return result;
            }

            /*@ multiply : forall M1 M2 . (c1:Color<M1>, c2:Color<M2>) : {MColor | true} */
            public static multiply(c1:Color, c2:Color) {
                var result = new Color(0, 0, 0);

                result.red = c1.red * c2.red;
                result.green = c1.green * c2.green;
                result.blue = c1.blue * c2.blue;

                return result;
            }

            /*@ multiplyScalar : forall M . (c1:Color<M>, f:number) : {MColor | true} */
            public static multiplyScalar(c1:Color, f:number) {
                var result = new Color(0, 0, 0);

                result.red = c1.red * f;
                result.green = c1.green * f;
                result.blue = c1.blue * f;

                return result;
            }


            /*@ divideFactor : forall M . (c1:Color<M>, f:{number | v != 0}) : {MColor | true} */
            public static divideFactor(c1:Color, f:number) {
                var result = new Color(0, 0, 0);

                result.red = c1.red / f;
                result.green = c1.green / f;
                result.blue = c1.blue / f;

                return result;
            }

            /*@ limit : (this:MColor) : {void | true} */
            public limit() {
                this.red = (this.red > 0) ? ((this.red > 1) ? 1 : this.red) : 0;
                this.green = (this.green > 0) ? ((this.green > 1) ? 1 : this.green) : 0;
                this.blue = (this.blue > 0) ? ((this.blue > 1) ? 1 : this.blue) : 0;
            }

            /*@ distance : forall M . (color:Color<M>) : {number | true} */
            public distance(color:Color) {
                var d = Math.abs(this.red - color.red) + Math.abs(this.green - color.green) + Math.abs(this.blue - color.blue);
                return d;
            }

            /*@ blend : forall M1 M2 . (c1:Color<M1>, c2:Color<M2>, w:number) : {MColor | true} */
            public static blend(c1:Color, c2:Color, w:number) {
                var result = new Color(0, 0, 0);
                result = Color.add(
                    Color.multiplyScalar(c1, 1 - w),
                    Color.multiplyScalar(c2, w)
                );
                return result;
            }

            public brightness() {
                var r = Math.floor(this.red * 255);
                var g = Math.floor(this.green * 255);
                var b = Math.floor(this.blue * 255);
                return (r * 77 + g * 150 + b * 29) / 256 //ORIG: >> 8;
            }

            public toString() {
                var r = Math.floor(this.red * 255);
                var g = Math.floor(this.green * 255);
                var b = Math.floor(this.blue * 255);

                return "rgb(" + r + "," + g + "," + b + ")";
            }
        }

        export class Light {
            /*@ position : MVector */
            public position:Vector;
            /*@ color : IColor */
            public color:Color;
            public intensity:number=10;

            /*@ new(position:MVector, color:IColor, intensity:number) => {Light<M> | true} */
            constructor(position:Vector, color:Color, intensity?) {
                this.position = position;
                this.color = color;
                this.intensity = intensity;
            }

            public toString() {
                return 'Light [' + this.position.x + ',' + this.position.y + ',' + this.position.z + ']';
            }
        }

        export class Vector {
            /*@ x : number */
            public x = 0;
            /*@ y : number */
            public y = 0;
            /*@ z : number */
            public z = 0;

            /*@ new(x:number, y:number, z:number) => {Vector<M> | true} */
            constructor(x?, y?, z?) {
                this.x = x;
                this.y = y;
                this.z = z;
            }

            /*@ copy : forall M . (this:MVector, vector:Vector<M>) : {void | true} */
            public copy(vector:Vector) {
                this.x = vector.x;
                this.y = vector.y;
                this.z = vector.z;
            }

            /*@ normalize : forall M . () : Vector<M> */
            public normalize() {
                var m = this.magnitude();
                if (m === 0) throw new Error("Cannot normalize the 0-vector!");
                return new Vector(this.x / m, this.y / m, this.z / m);
            }

            public magnitude() {
                var x = this.x;
                var y = this.y;
                var z = this.z;
                return Math.sqrt((x * x) + (y * y) + (z * z));
            }

            /*@ cross : forall M1 M . (w:Vector<M1>) : {Vector<M> | true} */
            public cross(w:Vector) {
                return new Vector(
                        -this.z * w.y + this.y * w.z,
                    this.z * w.x - this.x * w.z,
                        -this.y * w.x + this.x * w.y);
            }

            /*@ dot : forall M . (w:Vector<M>) : {number | true} */
            public dot(w:Vector) {
                return this.x * w.x + this.y * w.y + this.z * w.z;
            }

            /*@ add : forall M1 M2 M . (v:Vector<M1>, w:Vector<M2>) : {Vector<M> | true} */
            public static add(v:Vector, w:Vector) {
                return new Vector(w.x + v.x, w.y + v.y, w.z + v.z);
            }

            /*@ subtract : forall M1 M2 M . (v:Vector<M1>, w:Vector<M2>) : {Vector<M> | true} */
            public static subtract(v:Vector, w:Vector) {
                if (!w || !v) throw 'Vectors must be defined [' + v + ',' + w + ']';
                return new Vector(v.x - w.x, v.y - w.y, v.z - w.z);
            }

            /*@ multiplyVector : forall M1 M2 M . (v:Vector<M1>, w:Vector<M2>) : {Vector<M> | true} */
            public static multiplyVector(v:Vector, w:Vector) {
                return new Vector(v.x * w.x, v.y * w.y, v.z * w.z);
            }

            /*@ multiplyScalar : forall M1 M . (v:Vector<M1>, w:number) : {Vector<M> | true} */
            public static multiplyScalar(v:Vector, w:number) {
                return new Vector(v.x * w, v.y * w, v.z * w);
            }

            public toString() {
                return 'Vector [' + this.x + ',' + this.y + ',' + this.z + ']';
            }
        }

        export class Ray {
            /*@ position : MVector */
            public position:Vector;
            /*@ direction : MVector */
            public direction:Vector;

            /*@ new(position:MVector, direction:MVector) => {Ray<M> | true} */
            constructor(position:Vector, direction:Vector) {
                this.position = position;
                this.direction = direction;
            }

            public toString() {
                return 'Ray [' + this.position.toString() + ',' + this.direction.toString() + ']';
            }
        }

        export class Scene {
            public camera : Camera;
            /*@ shapes : IArray<Shape<Immutable>> */
            public shapes : Shape[];
            /*@ lights : IArray<Light<Immutable>> */
            public lights : Light[];
            public background : Background;

            constructor() {
                this.camera = new Camera(
                    new Vector(0, 0, -5),
                    new Vector(0, 0, 1),
                    new Vector(0, 1, 0)
                );
                this.shapes = new Array<Shape>(0);
                this.lights = new Array<Light>(0);
                this.background = new Background(new Color(0, 0, 1/2), 1/5);
            }
        }

        // module Material {

        export class BaseMaterial {
            public gloss:number = 2;
            public transparency:number = 0;
            public reflection:number = 0;
            public refraction:number = 1/2;
            public hasTexture:boolean = false;

            /*@ new(gloss:number, transparency:number, reflection:number, refraction:number, hasTexture:boolean) => {BaseMaterial<M> | true} */
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

            /*@ getColor : (u:number, v:number) : {IColor | true} */
            public getColor(u:number, v:number) : Color {
                throw "Abstract method";
            }

            /*@ wrapUp : (t:number) : {number | true} */
            public wrapUp(t:number) {
                t = t % 2;
                if (t < -1) t += 2;
                if (t >= 1) t -= 2;
                return t;
            }

            public toString() {
                return 'Material [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Solid extends BaseMaterial {
            public color:Color;

            /*@ new(color:IColor, reflection:number, refraction:number, transparency:number, gloss:number) => {Solid<M> | true} */
            constructor(color:Color, reflection:number, refraction:number, transparency:number, gloss:number) {
                super(gloss, transparency, reflection, refraction, false);
                this.color = color;
            }

            /*@ getColor : (u:number, v:number) : {IColor | true} */
            public getColor(u:number, v:number) : Color {
                return this.color;
            }

            public toString() {
                return 'SolidMaterial [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Chessboard extends BaseMaterial {
            public colorEven:Color;
            public colorOdd:Color;
            public density:number = 1/2;

            /*@ new(colorEven:IColor, colorOdd:IColor, reflection:number, transparency:number, gloss:number, density:number) => {Chessboard<M> | true} */
            constructor(colorEven:Color, colorOdd:Color, 
                        reflection:number, 
                        transparency:number, 
                        gloss:number, 
                        density?) {
                super(gloss, transparency, reflection, 1/2, true);
                this.colorEven = colorEven;
                this.colorOdd = colorOdd;
                this.density = density;
            }

            /*@ getColor : (u:number, v:number) : {IColor | true} */
            public getColor(u:number, v:number) : Color {
                var t = this.wrapUp(u * this.density) * this.wrapUp(v * this.density);

                if (t < 0)
                    return this.colorEven;
                else
                    return this.colorOdd;
            }

            public toString() {
                return 'ChessMaterial [gloss=' + this.gloss + ', transparency=' + this.transparency + ', hasTexture=' + this.hasTexture + ']';
            }
        }

        export class Shape {
            /*@ position : MVector */
            public position:Vector;
            public material:BaseMaterial;

            /*@ new(position:MVector, material:BaseMaterial<Immutable>) => {Shape<M> | true} */
            constructor(position:Vector, material:BaseMaterial) {
                this.position = position;
                this.material = material;
            }

            /*@ intersect : forall M . (ray:Ray<M>) : {MIntersectionInfo | true} */
            public intersect(ray:Ray) : IntersectionInfo {
                throw "Abstract method";
            }
        }

        export class Sphere extends Shape {
            public radius:number;

            /*@ new(position:MVector, radius:number, material:BaseMaterial<Immutable>) => {Sphere<M> | true} */
            constructor(position:Vector, radius:number, material:BaseMaterial) {
                super(position, material);
                this.radius = radius;
            }

            /*@ intersect : forall M . (ray:Ray<M>) : {MIntersectionInfo | true} */
            public intersect(ray:Ray) : IntersectionInfo {
                var info = new IntersectionInfo(false, 0, null, null, null, null, null);
                info.shape = <Shape>this;

                var dst = Vector.subtract(ray.position, this.position);

                var B = dst.dot(ray.direction);
                var C = dst.dot(dst) - (this.radius * this.radius);
                var D = (B * B) - C;

                if (D > 0) { // intersection!
                    info.isHit = true;
                    var infoDist = (-B) - Math.sqrt(D);
                    info.distance = infoDist;
                    var infoPos = Vector.add(
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

        export class Plane extends Shape {
            public d:number;

            /*@ new(position:MVector, d:number, material:BaseMaterial<Immutable>) => {Plane<M> | true} */
            constructor(position:Vector, d:number, material:BaseMaterial) {
                super(position, material);
                this.d = d;
            }

            /*@ intersect : forall M . (ray:Ray<M>) : {MIntersectionInfo | true} */
            public intersect(ray:Ray) : IntersectionInfo {
                var info = new IntersectionInfo(false, 0, null, null, null, null, null);

                var Vd = this.position.dot(ray.direction);
                if (Vd === 0) return info; // no intersection

                var t = -(this.position.dot(ray.position) + this.d) / Vd;
                if (t <= 0) return info;

                info.shape = <Shape>this;
                info.isHit = true;
                var infoPos = Vector.add(
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
                    var vU = new Vector(this.position.y, this.position.z, -this.position.x);
                    var vV = vU.cross(this.position);
                    var u = infoPos.dot(vU);
                    var v = infoPos.dot(vV);
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

        export class IntersectionInfo {
            /*@ isHit : boolean */
            public isHit = false;
            /*@ hitCount : number */
            public hitCount = 0;
            /*@ shape : Shape<ReadOnly>? */
            public shape = null;
            /*@ position : MVector? */
            public position = null;
            /*@ normal : MVector? */
            public normal = null;
            /*@ color : IColor? */
            public color = null;
            /*@ distance : number? */
            public distance = null;

            /*@ new(isHit:boolean,
                    hitCount:number,
                    shape:Shape<Immutable>?,
                    position:MVector?,
                    normal:MVector?,
                    color:IColor?,
                    distance:number?) => {IntersectionInfo<M> | true} */
            constructor(isHit?, hitCount?, shape?, position?, normal?, color?, distance?) {
                this.isHit = isHit;
                this.hitCount = hitCount;
                this.shape = shape;
                this.position = position;
                this.normal = normal;
                this.color = color;
                this.distance = distance;
            }

            /*@ initialize : (this:MIntersectionInfo) : {void | true} */
            public initialize() {
                this.color = new Color(0, 0, 0);
            }

            public toString() {
                var position = this.position;
                if (!position) return 'Intersection [position==null]';
                return 'Intersection [' + position.toString() + ']';
            }
        }

        export class Camera {
            public equator:Vector;
            public screen:Vector;

            /*@ position : MVector */
            public position:Vector;
            /*@ lookAt : MVector */
            public lookAt:Vector;
            /*@ up : MVector */
            public up:Vector;

            /*@ new(position:MVector, lookAt:MVector, up:MVector) => {Camera<M> | true} */
            constructor(position:Vector,
                        lookAt:Vector,
                        up:Vector) {
                this.equator = lookAt.normalize().cross(up);
                this.screen = Vector.add(position, lookAt);
                this.position = position;
                this.lookAt = lookAt;
                this.up = up;
            }

            /*@ getRay : forall M . (vx:number, vy:number) : {Ray<M> | true} */
            public getRay(vx:number, vy:number) {
                var pos = Vector.subtract(
                    this.screen,
                    Vector.subtract(
                        Vector.multiplyScalar(this.equator, vx),
                        Vector.multiplyScalar(this.up, vy)
                    )
                );
                pos.y = pos.y * -1;
                var dir = Vector.subtract(
                    pos,
                    this.position
                );

                var ray = new Ray(pos, dir.normalize());

                return ray;
            }
            
            public toString() {
                return 'Ray []';
            }
        }

        export class Background {
            /*@ color : MColor */
            public color:Color;
            public ambience:number = 0;

            /*@ new(color:MColor, ambience:number) => {Background<M> | true} */
            constructor(color:Color, ambience?) {
                this.color = color;
                this.ambience = ambience;
            }
        }

        /*@ extend :: (dest:[Mutable]{[s:string]:top}, src:[Immutable]{[s:string]:top}) => {[Mutable]{[s:string]:top} | true} */
        function extend(dest, src) {
            for (var p in src) {
                dest[p] = src[p];
            }
            return dest;
        }

        export class Engine {
            /*@ canvas : CanvasRenderingContext2D<Mutable>? */
            public canvas:CanvasRenderingContext2D = null; /* 2d context we can render to */
            /*@ options : EngineOptions */
            public options;

            /*@ new(options:EngineOptions) => {Engine<M> | true} */
            constructor(options) {
                // ORIG:
                // var this_options = extend({
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
                var this_options = options;

                this_options.canvasHeight /= this_options.pixelHeight;
                this_options.canvasWidth /= this_options.pixelWidth;

                this.options = this_options;

                /* TODO: dynamically include other scripts */
            }

            /*@ setPixel : forall M . (x:number, y:number, color:Color<M>) : {void | true} */
            public setPixel(x, y, color:Color) {
                var pxW, pxH;
                pxW = this.options.pixelWidth;
                pxH = this.options.pixelHeight;

                var canvas = this.canvas;
                if (canvas) {
                    (<CanvasRenderingContext2D>canvas).fillStyle = color.toString();
                    canvas.fillRect(x * pxW, y * pxH, pxW, pxH);
                } else {
                    if (x === y) {
                        checkNumber += color.brightness();
                    }
                    // print(x * pxW, y * pxH, pxW, pxH);
                }
            }

            /*@ renderScene : forall M1 M2 . (this:Engine<Mutable>, scene:Scene<M1>, canvas:HTMLCanvasElement<M2>?) : {void | true} */
            public renderScene(scene:Scene, canvas:HTMLCanvasElement) {
                checkNumber = 0;
                /* Get canvas */
                if (canvas) {
                    this.canvas = canvas.getContext("2d");
                } else {
                    this.canvas = null;
                }

                var canvasHeight = this.options.canvasHeight;
                var canvasWidth = this.options.canvasWidth;

                for (var y = 0; y < canvasHeight; y++) {
                    for (var x = 0; x < canvasWidth; x++) {
                        var yp = y * 1 / canvasHeight * 2 - 1;
                        var xp = x * 1 / canvasWidth * 2 - 1;

                        var ray = scene.camera.getRay(xp, yp);

                        var color = this.getPixelColor(ray, scene);

                        this.setPixel(x, y, color);
                    }
                }
                if (checkNumber !== 2321) {
                    throw new Error("Scene rendered incorrectly");
                }
            }

            /*@ getPixelColor : forall M1 M2 . (ray:Ray<M1>, scene:Scene<M2>) : {MColor | true} */
            public getPixelColor(ray:Ray, scene:Scene) {
                var info = this.testIntersection(ray, scene, null);
                if (info.isHit) {
                    var color = this.rayTrace(info, ray, scene, 0);
                    return color;
                }
                return scene.background.color;
            }

            /*@ testIntersection : forall M1 M2 M3 . (ray:Ray<M1>, scene:Scene<M2>, exclude:Shape<M3>?) : {MIntersectionInfo | true} */
            public testIntersection(ray:Ray, scene:Scene, exclude?:Shape) : IntersectionInfo {
                var hits = 0;
                /*@ best :: MIntersectionInfo */
                var best = new IntersectionInfo(false, 0, null, null, null, null, null);
                best.distance = 2000;

                var sceneShapes = scene.shapes;
                for (var i = 0; i < sceneShapes.length; i++) {
                    var shape = sceneShapes[i];

                    if (shape !== exclude) {
                        var info = shape.intersect(ray);
                        if (info.isHit && info.distance >= 0 && info.distance < best.distance) {
                            best = info;
                            hits++;
                        }
                    }
                }
                best.hitCount = hits;
                return best;
            }

            /*@ getReflectionRay : forall M1 M2 M . (P:MVector, N:Vector<M1>, V:Vector<M2>) : {Ray<M> | true} */
            public getReflectionRay(P:Vector, N:Vector, V:Vector) {
                var c1 = -N.dot(V);
                var R1 = Vector.add(
                    Vector.multiplyScalar(N, 2 * c1),
                    V
                );
                return new Ray(P, R1);
            }

            /*@ rayTrace : forall M1 M2 M3 . (info:IntersectionInfo<M1>, ray:Ray<M2>, scene:Scene<M3>, depth:number) : {MColor | true} */
            public rayTrace(info:IntersectionInfo, ray:Ray, scene:Scene, depth:number) {
                var infoColor = info.color;
                var infoShape = info.shape;
                var infoNormal = info.normal;
                var infoPosition = info.position;
                if (!infoColor || 
                    !infoShape || 
                    !infoNormal || 
                    !infoPosition) throw new Error('incomplete IntersectionInfo'); //TODO is there a way to get rid of this check?

                // Calc ambient
                var color = Color.multiplyScalar(infoColor, scene.background.ambience);
                var oldColor = color;
                var shininess = Math.pow(10, infoShape.material.gloss + 1);

                var sceneLights = scene.lights;
                for (var i = 0; i < sceneLights.length; i++) {
                    var light = sceneLights[i];

                    // Calc diffuse lighting
                    var v = Vector.subtract(
                        light.position,
                        infoPosition
                    ).normalize();

                    if (this.options.renderDiffuse) {
                        var L = v.dot(infoNormal);
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
                            var reflectionRay = this.getReflectionRay(infoPosition, infoNormal, ray.direction);
                            var refl = this.testIntersection(reflectionRay, scene, infoShape);

                            var reflColor;
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

                    var shadowInfo = new IntersectionInfo(false, 0, null, null, null, null, null);

                    if (this.options.renderShadows) {
                        var shadowRay = new Ray(infoPosition, v);

                        shadowInfo = this.testIntersection(shadowRay, scene, infoShape);
                        if (shadowInfo.isHit && shadowInfo.shape !== infoShape /*&& shadowInfo.shape.type != 'PLANE'*/) {
                            var vA = Color.multiplyScalar(color, 1/2);
                            var shadowInfoShape = shadowInfo.shape;
                            if (!shadowInfoShape) throw new Error('This should probably never happen');
                            var dB = (1/2 * Math.pow(shadowInfoShape.material.transparency, 1/2));
                            color = Color.addScalar(vA, dB);
                        }
                    }

                    // Phong specular highlights
                    if (this.options.renderHighlights && !shadowInfo.isHit && infoShape.material.gloss > 0) {
                        var Lv = Vector.subtract(
                            infoShape.position,
                            light.position
                        ).normalize();

                        var E = Vector.subtract(
                            scene.camera.position,
                            infoShape.position
                        ).normalize();

                        var H = Vector.subtract(
                            E,
                            Lv
                        ).normalize();

                        var glossWeight = Math.pow(Math.max(infoNormal.dot(H), 0), shininess);
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
            var scene = new Scene();

            scene.camera = new Camera(
                new Vector(0, 0, -15),
                new Vector(-1/5, 0, 5),
                new Vector(0, 1, 0)
            );

            scene.background = new Background(
                new Color(1/2, 1/2, 1/2),
                2/5
            );

            var sphere = new Sphere(
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

            var sphere1 = new Sphere(
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

            var plane = new Plane(
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
            scene.shapes = [<Shape>plane, <Shape>sphere, <Shape>sphere1];

            var light = new Light(
                new Vector(5, 10, -1),
                new Color(4/5, 4/5, 4/5),
                10 // (ORIG: default param omitted)
            );

            var light1 = new Light(
                new Vector(-3, 5, -15),
                new Color(4/5, 4/5, 4/5),
                100
            );

            // ORIG:
            // scene.lights.push(light);
            // scene.lights.push(light1);
            scene.lights = [light, light1];

            var imageWidth = 100; // $F('imageWidth');
            var imageHeight = 100; // $F('imageHeight');
            var pixelSize = [5,5];//"5,5".split(','); //  $F('pixelSize').split(',');
            var renderDiffuse = true; // $F('renderDiffuse');
            var renderShadows = true; // $F('renderShadows');
            var renderHighlights = true; // $F('renderHighlights');
            var renderReflections = true; // $F('renderReflections');
            var rayDepth = 2;//$F('rayDepth');

            var raytracer = new Engine(
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
            raytracer.renderScene(scene, null);
        }

    }
}
