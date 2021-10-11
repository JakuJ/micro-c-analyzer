int y; /* iterator, initialized to 0 */
int even; /* output variable */

even := 1;

if (x < 0) { x := x * (-1); }

while (y != x) {
    y := y + 1;
    even := 1 - even;
}

write even; /* even is 1 if x is an even number, otherwise 0 */