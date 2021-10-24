int x;
int y;
int i;
int inside;
int seed;

iters := 20000;

/* generator parameters */
a := 1103515245;
c := 12345;
m := 2147483648;

while (i < iters) {
    /* Create a pseudorandom coordinate in [-10000, 10000]*/
    seed := (a * seed + c) % m;
    x := seed % 10001;

    /* Create another pseudorandom coordinate */
    seed := (a * seed + c) % m;
    y := seed % 10001;

    /* Check if point (x, y) is inside the circle of radius 10000*/
    if (x * x + y * y <= 100000000) {
        inside := inside + 1;
    }

    i := i + 1;
}

/* Approximate 100000 * PI (we have no floating point numbers) */
write 100000 * 4 * inside / iters;
