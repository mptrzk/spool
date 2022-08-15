#ifndef MEM_H
#define MEM_H


#define VAL_U2(x) (*(unsigned short*) (mem_g + (x)))

#define MEM_SZ 768

#define IN_BUF_SZ 64
#define STACK_SZ 128

#define IN_BUF MEM_SZ - IN_BUF_SZ
#define STACK IN_BUF - STACK_SZ

#define PC_ADDR 0
#define PC_VAL VAL_U2(PC_ADDR)

#define SF_ADDR 2
#define SF_VAL VAL_U2(SF_ADDR)

#define SP_ADDR 4
#define SP_VAL VAL_U2(SP_ADDR)

#define MIN(a, b) (a) < (b) ? (a) : (b)


#endif
