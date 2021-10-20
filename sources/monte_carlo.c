int i;
int inside;

iters := 10000;

/* generator parameters */
seed := 123456789;
a := 1103515245;
c := 12345;
m := 2147483648;

while (i < iters) {
    /* Create a pseudorandom coordinate in [-10000, 10000]*/
    seed := (a * seed + c) % m;
    x := (20000 * seed) / m - 10000;

    /* Create another pseudorandom coordinate */
    seed := (a * seed + c) % m;
    y := 20000 * seed / m - 10000;

    /* Check if point (x, y) is inside the circle of radius 10000*/
    if (x * x + y * y <= 100000000) {
        inside := inside + 1;
    }

    i := i + 1;
}

/* Approximate 100000 * PI (we have no floating point numbers) */
write 100000 * 4 * inside / iters;
