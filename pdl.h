#include <stdint.h>
#include <stdio.h>
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
PDL_NUM_OPS(int8_t)
PDL_NUM_OPS(int16_t)
PDL_NUM_OPS(int32_t)
PDL_NUM_OPS(int64_t)
int32_t pn(int32_t i){printf("%d\n", i);return i;}
