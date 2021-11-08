int x;
int y;

while (y != x) {
    y := y - 1;
    continue;
    while (y == x) {
        y := y - 1;
        break;
    }
    while (y == x) {
        y := y - 1;
        continue;
    }
}

continue;