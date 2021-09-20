int i; /* iterator variable */
int num1;  
int num2;
int sum; /* output variable */
int count; /* input variable */

i := 1;
num1 := 0;
num2 := 1;
sum := 0;
read count;

while (i <= count) {
    write num1;
    sum := num1 + num2;
    num1 := num2;
    num2 := sum;
    i := i + 1;
}