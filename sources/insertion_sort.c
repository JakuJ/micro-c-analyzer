int[10] A;
int i;
int j;
int n;
int t;

A[0] := 34;
A[1] := -1;
A[2] := 54;
A[3] := 32;
A[4] := 76;
A[5] := 2034;
A[6] := -254;
A[7] := 5;
A[8] := 0;
A[9] := 1;
i := 1;
n := 10;

while (i < n) {
    j := i;
    while (j > 0 && A[j - 1] > A[j]) {
        t := A[j];
        A[j] := A[j - 1];
        A[j - 1] := t;
        j := j - 1;
    }
    i := i + 1;
}

i := 0;
while (i < n) {
    write A[i];
    i := i + 1;
}
