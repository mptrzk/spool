#ifndef OPCODES_H
#define OPCODES_H


#define LIT_BIT (0b1 << 7)
#define COND_BIT (0b1 << 6)

#define SIZE_BITS (0b11 << 4)

#define SIZE_1 (0b00 << 4)
#define SIZE_2 (0b01 << 4)
#define SIZE_4 (0b10 << 4)
#define SIZE_8 (0b11 << 4)

#define ID_LOC 0x0;
#define ID_LDR 0x1;
#define ID_STR 0x2;
#define ID_ADD 0x3;
#define ID_SUB 0x4;
#define ID_MUL 0x5;
#define ID_NOT 0x6;
#define ID_AND 0x7;
#define ID_OR  0x8;
#define ID_XOR 0x9;
#define ID_RAS 0xA;
#define ID_LAS 0xB;
#define ID_LLS 0xC;
#define ID_SWI 0xC;


#endif
