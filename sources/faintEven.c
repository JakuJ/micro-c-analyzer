int x; /* input to the program, read from the user */
int y; /* iterator, initialized to 0 */
int even; /* output variable */

read x;
even := 1;

if (x < 0) { x := x * (-1); }

while (y != x) {
    y := y + 1;
    even := 1 - even;
}

even := 0;

write even; /* even is 1 if x is an even number, otherwise 0 */