#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

const uint8_t pdl_i4=1;
typedef struct{uint8_t t;union{int32_t i4;};}pdl_t;

void wtf(char*m){printf("WTF! %s\n",m);exit(EXIT_FAILURE);}
int pdl_if(pdl_t a){
  if(a.t==pdl_i4)return a.i4?1:0;
  wtf("Bad if");return 0;
}
pdl_t pn(pdl_t a){
  if(a.t==pdl_i4)printf("%d\n",a.i4);
  else wtf("Bad pn");
  return a;
}
#define PDL_NUM_OP(name,op) \
  pdl_t name(pdl_t a,pdl_t b){ \
    pdl_t o; \
    if(a.t==pdl_i4&&b.t==pdl_i4){o.t=pdl_i4;o.i4=a.i4 op b.i4;} \
    else wtf(#op " takes numbers"); \
    return o; \
  }
PDL_NUM_OP(add,+)
PDL_NUM_OP(sub,-)
PDL_NUM_OP(mul,*)
PDL_NUM_OP(dv,/)
PDL_NUM_OP(eq,==)
PDL_NUM_OP(ne,!=)
PDL_NUM_OP(lt,<)
PDL_NUM_OP(gt,>)
PDL_NUM_OP(lte,<=)
PDL_NUM_OP(gte,>=)
