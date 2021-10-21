// one of the example programs in pa4fun
int x;
int y;
int i;

int[5] A;

A[0] := -1;
A[1] := 1;
A[2] := 15;
A[3] := -33;
A[4] := 8;

while (i < 5) {
    if (A[i] > 0) {
        x := x + A[i];
        i := i + 1;
    } else {
        y := y + A[i];
        i := i + 1;
    }
}
