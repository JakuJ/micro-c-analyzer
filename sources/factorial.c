int i; /* iterator variable */
int fact; /* output variable */
int num; /* input variable */

i := 1;
fact := 1;
read num;

while (i <= num) {
    fact := fact * i;
    i := i + 1;
}

write fact;