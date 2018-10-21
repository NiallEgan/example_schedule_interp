/*****************************************
  Emitting C Generated Code
*******************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
void f(void  x0) {
int32_t[5][5] x1;
int32_t[4][4] x2;
for(int x4=0; x4 < 4; x4++) {
int32_t x7 = x4 + 1;
for(int x5=0; x5 < 4; x5++) {
int32_t x6 = x5 + x4;
int32_t x8 = x5 + x7;
int32_t x9 = x6 + x8;
int32_t x10 = x7 + x5;
int32_t x11 = x9 + x10;
int32_t x12 = x5 + 1;
int32_t x13 = x12 + x7;
int32_t x14 = x11 + x13;
int32_t x15 = x14 / 4;
x2[x4][x5] = x15;

}

}
}
/*****************************************
  End of C Generated Code
*******************************************/
