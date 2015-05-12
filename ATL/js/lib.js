var x = 42;

function f() {
    return 128;
};

function g(x) {
    return x * x;
};

function Car() {
    this.wheels = 4;
    this.speed = 42;
    this.reads = false;

    this.getSpeed = function () {
        return this.speed;
    };
};

function Dog(age) {
    if (age)
        this.age = age;
    else
        this.age = 42;
    this.bark = function (s) {
        if (!s)
            s = ""
        console.log(s + " - age : " + this.age);
    }
};
