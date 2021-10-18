int[2] xs;
int n;

while (n < 5) {
    xs[0] := n;
    xs[1] := n * -2;
    if (n == 0 || n == 2 || n == 4) {
        n := n + 1;
    } else {
        n := n + 2;
    }
}

write n;
write xs[0];
write xs[1];
