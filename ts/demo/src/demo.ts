
// constants
const RATIO: number = 1/10;

// cannot re-use var name
let myname: string = "sam";

// can re-use var age again
var age: number = 10.2;

age = age * 3;

// Point is a type alias for {x:n, y:n}
type TPoint = {
    x: number;
    y: number;
};

let p = {x:10, y:12};

// an extendable structural type
interface IPoint {
    x: number;
    y: number;
}

// generic function
function say<T>(arg: T): T {
    return arg;
}

class Point {
    x: number;
    y: number;

    // Normal signature with defaults
    constructor(x = 0, y = 0) {
        this.x = x;
        this.y = y;
    }
}

const p1 = new Point();
let p2 = new Point(1, 2);

class Point3D extends Point {
    z: number;

    constructor(x = 0, y = 0, z = 0) {
        super(); // must calll super here
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

let p3 = new Point3D(1, 2, 3);

interface Nameable {
    get name(): string;
    set name(value);
}

class Person0 implements Nameable {
    private _name: string;
    private _age: number;

    get name() {
        return this._name;
    }
    set name(value) {
        this._name = value;
    }

    get age() {
        return this._age;
    }
    set age(value) {
        this._age = value;
    }
}


abstract class Party {    
    abstract get name(): string;
    abstract set name(value);
}

class Person implements Party {
    private _name: string;
    private _age: number;

    get name() {
        return this._name;
    }
    set name(value) {
        this._name = value;
    }

    get age() {
        return this._age;
    }
    set age(value) {
        this._age = value;
    }
}

let sam = new Person();
sam.name = "sam";
sam.age = 12;

// variadic function parameter (rest param)
function multiply(n: number, ...m: number[]) {
    return m.map(x => n * x);
}

var result = multiply(3, 1, 2, 3, 5);

type f1 = (x: number) => number;
