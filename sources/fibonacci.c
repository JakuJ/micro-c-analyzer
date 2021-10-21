int sum; /* output variable */

i := 1; /* iterator variable */
num1 := 0;
num2 := 1;
read count;

if (count >= 0) {
    while (i <= count) {
        write num1;
        sum := num1 + num2;
        num1 := num2;
        num2 := sum;
        i := i + 1;
    }
}
