#include <stdint.h>
#include <stdio.h>
typedef int8_t i1;
typedef int16_t i2;
typedef int32_t i4;
typedef int64_t i8;
#define PDL_NUM_OP(t,n,o)t n##_##t(t a,t b){return a o b;}
#define PDL_NUM_OPS(t) \
  PDL_NUM_OP(t,add,+) \
  PDL_NUM_OP(t,sub,-) \
  PDL_NUM_OP(t,mul,*) \
  PDL_NUM_OP(t,dv,/) \
  PDL_NUM_OP(t,eq,==) \
  PDL_NUM_OP(t,ne,!=) \
  PDL_NUM_OP(t,lt,<) \
  PDL_NUM_OP(t,gt,>) \
  PDL_NUM_OP(t,lte,<=) \
  PDL_NUM_OP(t,gte,>=)
PDL_NUM_OPS(i1)
PDL_NUM_OPS(i2)
PDL_NUM_OPS(i4)
PDL_NUM_OPS(i8)
i4 pn(i4 i){printf("%d\n", i);return i;}
