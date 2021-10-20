/* more or less the program from figure 5.20 (Appetizer book) */

int x;
int y;
int i;
int[10] A;

A[2] := 18;
A[1] := 3;
A[3] := 42;
A[7] := 69;
A[9] := -15;

while (i < 10) {
    z := A[i];
    if (z > 0) {
        x := x + z;
        y := y + 1;
    }
    i := i + 1;
}

avg := x / y;

write avg;
