#include <stdint.h>
#include <stdio.h>

/*struct pdl_box{
  uint8_t type;
  union{
    int32_t i32;
  };
};*/

int32_t add(int32_t a,int32_t b){return a+b;}
int32_t sub(int32_t a,int32_t b){return a-b;}
int32_t mul(int32_t a,int32_t b){return a*b;}
int32_t div(int32_t a,int32_t b){return a/b;}
int32_t eq(int32_t a,int32_t b){return a==b;}
int32_t ne(int32_t a,int32_t b){return a!=b;}
int32_t lt(int32_t a,int32_t b){return a<b;}
int32_t gt(int32_t a,int32_t b){return a>b;}
int32_t lte(int32_t a,int32_t b){return a<=b;}
int32_t gte(int32_t a,int32_t b){return a>=b;}
int32_t pn(int32_t i){printf("%d\n", i);return i;}
