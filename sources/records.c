{ int real; int imag } C;
{ int fst; int snd; int trd } R;

R := (1,2,3);
C := (-3,5);

write R.fst;
write R.snd;
write R.trd;

C.real := 3 - 2;

write (C.real + C.imag);
