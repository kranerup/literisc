;;; Computing Hangul syllable names_START:
        rx= 32768 sp
        jsr main
_HALT:
        j _halt
        ; ======== function rd_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 16, params: 2, max-reg: 9, leaf: no, vreg: yes, spills: 4
RD_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -24 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 16 p0
        m[a+n]=rx 20 p1
        ; 1903: address = 12887;
        ; 1904: address += idx*8;
        rx= 12887 r6
        ; 1905: int i, j;
        rx=m[a+n] 16 r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        ; 1906: int prev = readFromDevice(address,0);
        ; 1907: for (i = 0; i < 4; i++) {
        rx= 65536 r0
        a+=rx r0
        rx=m[a] r9
        ; 1914: }
        rx= 0 r7
FOR10:
        rx= 4 r0
        a=rx r7
        a-=rx r0
        jge endfor12
        ; 1909: int merged = (prev >> 4) | (curr << 28);
        ; -- inline begin: result in __inline_result_i354 --
        a= 1
        a+=rx r6
        rx=a r0
        a=rx r7
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
        rx= 65536 r0
        rx=m[a+n] 4 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 0 r0
INLINE_RET_i354:
        a=rx sp
        rx=m[a+n] 0 r0
        m[a+n]=rx 12 r0
        ; 1910: prev = curr;
        a=rx r9
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r0
        a=rx sp
        rx=m[a+n] 12 r1
        a=rx r1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a=a<<1
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 8 r0
        rx=m[a+n] 12 r0
        a=rx r0
        rx=a r9
        ; 1913: }
        rx= 0 r8
FOR13:
        rx= 4 r0
        a=rx r8
        a-=rx r0
        jge endfor15
        ; 1913: }
        a=rx sp
        rx=m[a+n] 8 r0
        a=rx r8
        a=a<<1
        a=a<<1
        a=a<<1
        rx=a r2
        a=rx r0
SHRLOOP16:
        rx=a r1
        a=rx r2
        rx= 0 r3
        a-=rx r3
        jz shrend17
        a=rx r2
        rx= -1 r3
        a+=rx r3
        rx=a r2
        a=rx r1
        a=a>>1
        j shrloop16
SHREND17:
        a=rx r1
        mask-a-b
        rx=a r0
        a=rx sp
        rx=m[a+n] 20 r1
        a=rx r7
        a=a<<1
        a=a<<1
        rx=a r2
        a=rx r8
        a+=rx r2
        a+=rx r1
        m[a].b=rx r0
FORCONT14:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for13
ENDFOR15:
FORCONT11:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for10
ENDFOR12:
RD_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END1:
        a=rx sp
        rx= 24 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        ; ======== function wr_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 28, params: 2, max-reg: 9, leaf: no, vreg: yes, spills: 7
WR_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -36 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 28 p0
        m[a+n]=rx 32 p1
        ; 1917: address = 12887;
        ; 1918: address += idx*8;
        rx= 12887 r6
        ; 1919: int i, j;
        rx=m[a+n] 28 r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        ; 1920: int v;
        ; 1921: int wprev = readFromDevice(address,0) & 0xf;
        ; 1922: int wlast = readFromDevice(address+4,0) & 0xfffffff0;
        rx= 65536 r0
        a+=rx r0
        rx=m[a] r0
        rx= 15 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 24 r0
        ; 1923: for (i = 0; i < 4; i++) {
        ; -- inline begin: result in __inline_result_i356 --
        a= 4
        a+=rx r6
        rx=a r0
        a=rx sp
        m[a+n]=rx 12 r0
        rx= 65536 r0
        rx=m[a+n] 12 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 8 r0
INLINE_RET_i356:
        a=rx sp
        rx=m[a+n] 8 r0
        rx= 4294967280 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 20 r0
        ; 1933: writeToDevice(address+4, wprev, 0);
        rx= 0 r7
FOR35:
        rx= 4 r0
        a=rx r7
        a-=rx r0
        jge endfor37
        ; 1925: for (j = 0; j < 4; j++) {
        rx= 3 r0
        a=rx r7
        a-=rx r0
        jz cmptrue40
        a= 0
        j cmpend41
CMPTRUE40:
        a= 1
CMPEND41:
        rx= 0 r0
        a-=rx r0
        jz ternelse38
        a=rx sp
        rx=m[a+n] 20 r0
        a=rx r0
        j ternend39
TERNELSE38:
        a= 0
TERNEND39:
        rx=a r0
        a=rx sp
        m[a+n]=rx 16 r0
        ; 1930: writeToDevice(address+i, wprev, 0);
        rx= 0 r8
FOR42:
        rx= 4 r0
        a=rx r8
        a-=rx r0
        jge endfor44
        ; 1927: wprev |= v << (4 + j*8);
        a=rx sp
        rx=m[a+n] 32 r0
        a=rx r7
        a=a<<1
        a=a<<1
        rx=a r1
        a=rx r8
        a+=rx r1
        a+=rx r0
        rx=m[a].b r0
        a=rx r0
        rx=a r9
        ; 1928: if (28 > j*8) wcurr |= v >> (28 - j*8);
        a=rx sp
        rx=m[a+n] 24 r0
        a=rx r9
        rx=a r1
        rx= 4 r2
        a=rx r8
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r3
        a=rx r1
SHLLOOP45:
        rx=a r2
        a=rx r3
        rx= 0 r4
        a-=rx r4
        jz shlend46
        a=rx r3
        rx= -1 r4
        a+=rx r4
        rx=a r3
        a=rx r2
        a=a<<1
        j shlloop45
SHLEND46:
        a=rx r2
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 24 r0
        ; 1929: }
        rx= 28 r0
        a=rx r8
        a=a<<1
        a=a<<1
        a=a<<1
        rx=a r1
        a=rx r0
        rx=a r2
        a=rx r1
        rx=a r0
        a=rx r2
        rx=a r1
        a=rx r0
        a-=rx r1
        jge endif48
        ; 1929: }
        a=rx sp
        rx=m[a+n] 16 r0
        a=rx r9
        rx=a r1
        rx= 28 r2
        a=rx r8
        a=a<<1
        a=a<<1
        a=a<<1
        rx=a r3
        a=rx r2
        a-=rx r3
        rx=a r3
        a=rx r1
SHRLOOP49:
        rx=a r2
        a=rx r3
        rx= 0 r4
        a-=rx r4
        jz shrend50
        a=rx r3
        rx= -1 r4
        a+=rx r4
        rx=a r3
        a=rx r2
        a=a>>1
        j shrloop49
SHREND50:
        a=rx r2
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 16 r0
ENDIF48:
FORCONT43:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for42
ENDFOR44:
        ; 1931: wprev = wcurr;
        ; -- inline begin: result in __inline_result_i357 --
        a=rx r7
        a+=rx r6
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
        rx=m[a+n] 24 r0
        rx= 65536 r1
        rx=m[a+n] 4 r2
        a=rx r2
        a+=rx r1
        m[a]=rx r0
INLINE_RET_i357:
        a=rx sp
        rx=m[a+n] 4 r0
        ; -- inline end --
        ; 1932: }
        rx=m[a+n] 16 r0
        m[a+n]=rx 24 r0
FORCONT36:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for35
ENDFOR37:
        ; 1934: }
        ; -- inline begin: result in __inline_result_i358 --
        a= 4
        a+=rx r6
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        rx=m[a+n] 24 r0
        rx= 65536 r1
        rx=m[a+n] 0 r2
        a=rx r2
        a+=rx r1
        m[a]=rx r0
INLINE_RET_i358:
        a=rx sp
        rx=m[a+n] 0 r0
        ; -- inline end --
WR_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END18:
        a=rx sp
        rx= 36 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        lalign-dword 0
G_PASS:
        adword 0
        lalign-dword 0
G_FAIL:
        adword 0
        ; ======== function test_CoreTickConfiguration ========
        ; frame-size: 132, params: 1, max-reg: 5, leaf: yes, vreg: yes, spills: 33
TEST_CORETICKCONFIGURATION:
        push-r r5
        a=rx sp
        rx= -132 r0
        a+=rx r0
        rx=a sp
        ; 12382: wr_CoreTickConfiguration_clkDivider(0x64);
        ; 12383: wr_CoreTickConfiguration_stepDivider(0xa);
        ; -- inline begin: result in __inline_result_i2322 --
        ; 269: address = 2;
        ; 270: int entry;
        rx= 2 r2
        ; 271: entry = readFromDevice(address,0);
        ; 272: entry = (entry & ~(int)0x1ffff) | ((int)(clkDivider & 0x1ffff));
        rx= 65536 r3
        a=rx r2
        a+=rx r3
        rx=m[a] r3
        ; 273: writeToDevice(address,entry,0);
        rx= -131072 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        rx= 100 r5
        a=rx r5
        a|=rx r4
        rx=a r3
        rx= 65536 r4
        a=rx r2
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2324:
        ; -- inline end --
INLINE_RET_i2322:
        ; -- inline end --
        ; 12384: rd_CoreTickConfiguration_clkDivider(&val);
        ; -- inline begin: result in __inline_result_i2325 --
        ; 284: address = 2;
        ; 285: int entry;
        rx= 2 r2
        a=rx sp
        m[a+n]=rx 124 r2
        ; 286: entry = readFromDevice(address,0);
        ; 287: entry = (entry & ~((int)0xf << 17)) | ((int)(stepDivider & 0xf) << 17);
        rx= 65536 r2
        rx=m[a+n] 124 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 120 r2
        rx=m[a+n] 120 r2
        rx= -1966081 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 1310720 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 120 r2
        rx=m[a+n] 120 r2
        rx= 65536 r3
        rx=m[a+n] 124 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2327:
        a=rx sp
        rx=m[a+n] 120 r2
        ; -- inline end --
INLINE_RET_i2325:
        a=rx sp
        rx=m[a+n] 128 r2
        ; -- inline end --
        ; 12385: rd_CoreTickConfiguration_stepDivider(&val);
        ; -- inline begin: result in __inline_result_i2328 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 116 r2
        ; 262: address = 2;
        ; 263: int entry;
        rx= 2 r2
        m[a+n]=rx 112 r2
        ; 264: entry = readFromDevice(address,0);
        ; 265: *out = (int)((entry) & 0x1ffff);
        rx= 65536 r2
        rx=m[a+n] 112 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 108 r2
        rx=m[a+n] 108 r2
        rx= 131071 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 116 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2328:
        a=rx sp
        rx=m[a+n] 116 r2
        ; -- inline end --
        ; 12386: wr_CoreTickConfiguration_clkDivider(0x1234 & 0x1ffff);
        ; -- inline begin: result in __inline_result_i2330 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 104 r2
        ; 277: address = 2;
        ; 278: int entry;
        rx= 2 r2
        m[a+n]=rx 100 r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        rx= 65536 r2
        rx=m[a+n] 100 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 96 r2
        rx=m[a+n] 96 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 15 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 104 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2330:
        a=rx sp
        rx=m[a+n] 104 r2
        ; -- inline end --
        ; 12387: rd_CoreTickConfiguration_clkDivider(&val);
        ; -- inline begin: result in __inline_result_i2332 --
        rx= 4660 r2
        rx= 131071 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 92 r2
        ; 269: address = 2;
        ; 270: int entry;
        rx= 2 r2
        m[a+n]=rx 88 r2
        ; 271: entry = readFromDevice(address,0);
        ; 272: entry = (entry & ~(int)0x1ffff) | ((int)(clkDivider & 0x1ffff));
        rx= 65536 r2
        rx=m[a+n] 88 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 84 r2
        rx=m[a+n] 84 r2
        rx= -131072 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 92 r3
        rx= 131071 r4
        a=rx r4
        a&=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 84 r2
        rx=m[a+n] 84 r2
        rx= 65536 r3
        rx=m[a+n] 88 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2334:
        a=rx sp
        rx=m[a+n] 84 r2
        ; -- inline end --
INLINE_RET_i2332:
        a=rx sp
        rx=m[a+n] 92 r2
        ; -- inline end --
        ; 12388: rd_CoreTickConfiguration_stepDivider(&val);
        ; -- inline begin: result in __inline_result_i2335 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 80 r2
        ; 262: address = 2;
        ; 263: int entry;
        rx= 2 r2
        m[a+n]=rx 76 r2
        ; 264: entry = readFromDevice(address,0);
        ; 265: *out = (int)((entry) & 0x1ffff);
        rx= 65536 r2
        rx=m[a+n] 76 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 72 r2
        rx=m[a+n] 72 r2
        rx= 131071 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 80 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2335:
        a=rx sp
        rx=m[a+n] 80 r2
        ; -- inline end --
        ; 12389: wr_CoreTickConfiguration_stepDivider(0xf);
        ; -- inline begin: result in __inline_result_i2337 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 68 r2
        ; 277: address = 2;
        ; 278: int entry;
        rx= 2 r2
        m[a+n]=rx 64 r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        rx= 65536 r2
        rx=m[a+n] 64 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 60 r2
        rx=m[a+n] 60 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 15 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 68 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2337:
        a=rx sp
        rx=m[a+n] 68 r2
        ; -- inline end --
        ; 12390: rd_CoreTickConfiguration_stepDivider(&val);
        ; -- inline begin: result in __inline_result_i2339 --
        ; 284: address = 2;
        ; 285: int entry;
        rx= 2 r2
        m[a+n]=rx 56 r2
        ; 286: entry = readFromDevice(address,0);
        ; 287: entry = (entry & ~((int)0xf << 17)) | ((int)(stepDivider & 0xf) << 17);
        rx= 65536 r2
        rx=m[a+n] 56 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 52 r2
        rx=m[a+n] 52 r2
        rx= -1966081 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 1966080 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 52 r2
        rx=m[a+n] 52 r2
        rx= 65536 r3
        rx=m[a+n] 56 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2341:
        a=rx sp
        rx=m[a+n] 52 r2
        ; -- inline end --
INLINE_RET_i2339:
        a=rx sp
        rx=m[a+n] 60 r2
        ; -- inline end --
        ; 12391: rd_CoreTickConfiguration_clkDivider(&val);
        ; -- inline begin: result in __inline_result_i2342 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 48 r2
        ; 277: address = 2;
        ; 278: int entry;
        rx= 2 r2
        m[a+n]=rx 44 r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        rx= 65536 r2
        rx=m[a+n] 44 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 40 r2
        rx=m[a+n] 40 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 15 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 48 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2342:
        a=rx sp
        rx=m[a+n] 48 r2
        ; -- inline end --
        ; 12392: wr_CoreTickConfiguration_stepDivider(0xff);
        ; -- inline begin: result in __inline_result_i2344 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 36 r2
        ; 262: address = 2;
        ; 263: int entry;
        rx= 2 r2
        m[a+n]=rx 32 r2
        ; 264: entry = readFromDevice(address,0);
        ; 265: *out = (int)((entry) & 0x1ffff);
        rx= 65536 r2
        rx=m[a+n] 32 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 28 r2
        rx=m[a+n] 28 r2
        rx= 131071 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 36 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2344:
        a=rx sp
        rx=m[a+n] 36 r2
        ; -- inline end --
        ; 12393: rd_CoreTickConfiguration_stepDivider(&val);
        ; -- inline begin: result in __inline_result_i2346 --
        ; 284: address = 2;
        ; 285: int entry;
        rx= 2 r2
        m[a+n]=rx 24 r2
        ; 286: entry = readFromDevice(address,0);
        ; 287: entry = (entry & ~((int)0xf << 17)) | ((int)(stepDivider & 0xf) << 17);
        rx= 65536 r2
        rx=m[a+n] 24 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        rx= -1966081 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 1966080 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        rx= 65536 r3
        rx=m[a+n] 24 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2348:
        a=rx sp
        rx=m[a+n] 20 r2
        ; -- inline end --
INLINE_RET_i2346:
        a=rx sp
        rx=m[a+n] 28 r2
        ; -- inline end --
        ; 12394: }
        ; -- inline begin: result in __inline_result_i2349 --
        rx= 128 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 16 r2
        ; 277: address = 2;
        ; 278: int entry;
        rx= 2 r2
        m[a+n]=rx 12 r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        rx= 65536 r2
        rx=m[a+n] 12 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 8 r2
        rx=m[a+n] 8 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 15 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 16 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2349:
        a=rx sp
        rx=m[a+n] 16 r2
        ; -- inline end --
TEST_CORETICKCONFIGURATION_END51:
        a=rx sp
        rx= 132 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        a=rx srp
        j-a
        ; ======== function test_ERMRedConfiguration ========
        ; frame-size: 196, params: 1, max-reg: 5, leaf: yes, vreg: yes, spills: 49
TEST_ERMREDCONFIGURATION:
        push-r r5
        a=rx sp
        rx= -196 r0
        a+=rx r0
        rx=a sp
        ; 12408: wr_ERMRedConfiguration_redXoff(0xcc);
        ; 12409: wr_ERMRedConfiguration_redXon(0x200);
        ; -- inline begin: result in __inline_result_i2371 --
        ; 607: address = 57850;
        ; 608: int entry;
        rx= 57850 r2
        ; 609: entry = readFromDevice(address,0);
        ; 610: entry = (entry & ~(int)0xfff) | ((int)(redXoff & 0xfff));
        rx= 65536 r3
        a=rx r2
        a+=rx r3
        rx=m[a] r3
        ; 611: writeToDevice(address,entry,0);
        rx= -4096 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        rx= 204 r5
        a=rx r5
        a|=rx r4
        rx=a r3
        rx= 65536 r4
        a=rx r2
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2373:
        ; -- inline end --
INLINE_RET_i2371:
        ; -- inline end --
        ; 12410: wr_ERMRedConfiguration_redMaxCells(0xf);
        ; -- inline begin: result in __inline_result_i2374 --
        ; 622: address = 57850;
        ; 623: int entry;
        rx= 57850 r2
        a=rx sp
        m[a+n]=rx 188 r2
        ; 624: entry = readFromDevice(address,0);
        ; 625: entry = (entry & ~((int)0xfff << 12)) | ((int)(redXon & 0xfff) << 12);
        rx= 65536 r2
        rx=m[a+n] 188 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 184 r2
        rx=m[a+n] 184 r2
        rx= -16773121 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 2097152 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 184 r2
        rx=m[a+n] 184 r2
        rx= 65536 r3
        rx=m[a+n] 188 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2376:
        a=rx sp
        rx=m[a+n] 184 r2
        ; -- inline end --
INLINE_RET_i2374:
        a=rx sp
        rx=m[a+n] 192 r2
        ; -- inline end --
        ; 12411: rd_ERMRedConfiguration_redXoff(&val);
        ; -- inline begin: result in __inline_result_i2377 --
        ; 637: address = 57850;
        ; 638: int entry;
        rx= 57850 r2
        m[a+n]=rx 180 r2
        ; 639: entry = readFromDevice(address,0);
        ; 640: entry = (entry & ~((int)0xff << 24)) | ((int)(redMaxCells & 0xff) << 24);
        rx= 65536 r2
        rx=m[a+n] 180 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 176 r2
        rx=m[a+n] 176 r2
        rx= 16777215 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 251658240 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 176 r2
        rx=m[a+n] 176 r2
        rx= 65536 r3
        rx=m[a+n] 180 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2379:
        a=rx sp
        rx=m[a+n] 176 r2
        ; -- inline end --
INLINE_RET_i2377:
        a=rx sp
        rx=m[a+n] 184 r2
        ; -- inline end --
        ; 12412: rd_ERMRedConfiguration_redXon(&val);
        ; -- inline begin: result in __inline_result_i2380 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 172 r2
        ; 600: address = 57850;
        ; 601: int entry;
        rx= 57850 r2
        m[a+n]=rx 168 r2
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 168 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 164 r2
        rx=m[a+n] 164 r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 172 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2380:
        a=rx sp
        rx=m[a+n] 172 r2
        ; -- inline end --
        ; 12413: rd_ERMRedConfiguration_redMaxCells(&val);
        ; -- inline begin: result in __inline_result_i2382 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 160 r2
        ; 615: address = 57850;
        ; 616: int entry;
        rx= 57850 r2
        m[a+n]=rx 156 r2
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 156 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 152 r2
        rx=m[a+n] 152 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 160 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2382:
        a=rx sp
        rx=m[a+n] 160 r2
        ; -- inline end --
        ; 12414: wr_ERMRedConfiguration_redXoff(0xabc);
        ; -- inline begin: result in __inline_result_i2384 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 148 r2
        ; 630: address = 57850;
        ; 631: int entry;
        rx= 57850 r2
        m[a+n]=rx 144 r2
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        rx= 65536 r2
        rx=m[a+n] 144 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 140 r2
        rx=m[a+n] 140 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 255 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 148 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2384:
        a=rx sp
        rx=m[a+n] 148 r2
        ; -- inline end --
        ; 12415: rd_ERMRedConfiguration_redXoff(&val);
        ; -- inline begin: result in __inline_result_i2386 --
        ; 607: address = 57850;
        ; 608: int entry;
        rx= 57850 r2
        m[a+n]=rx 136 r2
        ; 609: entry = readFromDevice(address,0);
        ; 610: entry = (entry & ~(int)0xfff) | ((int)(redXoff & 0xfff));
        rx= 65536 r2
        rx=m[a+n] 136 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 132 r2
        rx=m[a+n] 132 r2
        rx= -4096 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 2748 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 132 r2
        rx=m[a+n] 132 r2
        rx= 65536 r3
        rx=m[a+n] 136 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2388:
        a=rx sp
        rx=m[a+n] 132 r2
        ; -- inline end --
INLINE_RET_i2386:
        a=rx sp
        rx=m[a+n] 140 r2
        ; -- inline end --
        ; 12416: rd_ERMRedConfiguration_redXon(&val);
        ; -- inline begin: result in __inline_result_i2389 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 128 r2
        ; 600: address = 57850;
        ; 601: int entry;
        rx= 57850 r2
        m[a+n]=rx 124 r2
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 124 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 120 r2
        rx=m[a+n] 120 r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 128 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2389:
        a=rx sp
        rx=m[a+n] 128 r2
        ; -- inline end --
        ; 12417: rd_ERMRedConfiguration_redMaxCells(&val);
        ; -- inline begin: result in __inline_result_i2391 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 116 r2
        ; 615: address = 57850;
        ; 616: int entry;
        rx= 57850 r2
        m[a+n]=rx 112 r2
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 112 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 108 r2
        rx=m[a+n] 108 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 116 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2391:
        a=rx sp
        rx=m[a+n] 116 r2
        ; -- inline end --
        ; 12418: wr_ERMRedConfiguration_redXon(0x7ff);
        ; -- inline begin: result in __inline_result_i2393 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 104 r2
        ; 630: address = 57850;
        ; 631: int entry;
        rx= 57850 r2
        m[a+n]=rx 100 r2
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        rx= 65536 r2
        rx=m[a+n] 100 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 96 r2
        rx=m[a+n] 96 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 255 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 104 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2393:
        a=rx sp
        rx=m[a+n] 104 r2
        ; -- inline end --
        ; 12419: rd_ERMRedConfiguration_redXon(&val);
        ; -- inline begin: result in __inline_result_i2395 --
        ; 622: address = 57850;
        ; 623: int entry;
        rx= 57850 r2
        m[a+n]=rx 92 r2
        ; 624: entry = readFromDevice(address,0);
        ; 625: entry = (entry & ~((int)0xfff << 12)) | ((int)(redXon & 0xfff) << 12);
        rx= 65536 r2
        rx=m[a+n] 92 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 88 r2
        rx=m[a+n] 88 r2
        rx= -16773121 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 8384512 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 88 r2
        rx=m[a+n] 88 r2
        rx= 65536 r3
        rx=m[a+n] 92 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2397:
        a=rx sp
        rx=m[a+n] 88 r2
        ; -- inline end --
INLINE_RET_i2395:
        a=rx sp
        rx=m[a+n] 96 r2
        ; -- inline end --
        ; 12420: rd_ERMRedConfiguration_redXoff(&val);
        ; -- inline begin: result in __inline_result_i2398 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 84 r2
        ; 615: address = 57850;
        ; 616: int entry;
        rx= 57850 r2
        m[a+n]=rx 80 r2
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 80 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 76 r2
        rx=m[a+n] 76 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 84 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2398:
        a=rx sp
        rx=m[a+n] 84 r2
        ; -- inline end --
        ; 12421: rd_ERMRedConfiguration_redMaxCells(&val);
        ; -- inline begin: result in __inline_result_i2400 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 72 r2
        ; 600: address = 57850;
        ; 601: int entry;
        rx= 57850 r2
        m[a+n]=rx 68 r2
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 68 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 64 r2
        rx=m[a+n] 64 r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 72 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2400:
        a=rx sp
        rx=m[a+n] 72 r2
        ; -- inline end --
        ; 12422: wr_ERMRedConfiguration_redMaxCells(0xaa);
        ; -- inline begin: result in __inline_result_i2402 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 60 r2
        ; 630: address = 57850;
        ; 631: int entry;
        rx= 57850 r2
        m[a+n]=rx 56 r2
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        rx= 65536 r2
        rx=m[a+n] 56 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 52 r2
        rx=m[a+n] 52 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 255 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 60 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2402:
        a=rx sp
        rx=m[a+n] 60 r2
        ; -- inline end --
        ; 12423: rd_ERMRedConfiguration_redMaxCells(&val);
        ; -- inline begin: result in __inline_result_i2404 --
        ; 637: address = 57850;
        ; 638: int entry;
        rx= 57850 r2
        m[a+n]=rx 48 r2
        ; 639: entry = readFromDevice(address,0);
        ; 640: entry = (entry & ~((int)0xff << 24)) | ((int)(redMaxCells & 0xff) << 24);
        rx= 65536 r2
        rx=m[a+n] 48 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 44 r2
        rx=m[a+n] 44 r2
        rx= 16777215 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= -1442840576 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 44 r2
        rx=m[a+n] 44 r2
        rx= 65536 r3
        rx=m[a+n] 48 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2406:
        a=rx sp
        rx=m[a+n] 44 r2
        ; -- inline end --
INLINE_RET_i2404:
        a=rx sp
        rx=m[a+n] 52 r2
        ; -- inline end --
        ; 12424: rd_ERMRedConfiguration_redXoff(&val);
        ; -- inline begin: result in __inline_result_i2407 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 40 r2
        ; 630: address = 57850;
        ; 631: int entry;
        rx= 57850 r2
        m[a+n]=rx 36 r2
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        rx= 65536 r2
        rx=m[a+n] 36 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 32 r2
        rx=m[a+n] 32 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 255 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 40 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2407:
        a=rx sp
        rx=m[a+n] 40 r2
        ; -- inline end --
        ; 12425: rd_ERMRedConfiguration_redXon(&val);
        ; -- inline begin: result in __inline_result_i2409 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 28 r2
        ; 600: address = 57850;
        ; 601: int entry;
        rx= 57850 r2
        m[a+n]=rx 24 r2
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 24 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 28 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2409:
        a=rx sp
        rx=m[a+n] 28 r2
        ; -- inline end --
        ; 12426: }
        ; -- inline begin: result in __inline_result_i2411 --
        rx= 192 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 16 r2
        ; 615: address = 57850;
        ; 616: int entry;
        rx= 57850 r2
        m[a+n]=rx 12 r2
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        rx= 65536 r2
        rx=m[a+n] 12 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 8 r2
        rx=m[a+n] 8 r2
        a=rx r2
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 16 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2411:
        a=rx sp
        rx=m[a+n] 16 r2
        ; -- inline end --
TEST_ERMREDCONFIGURATION_END52:
        a=rx sp
        rx= 196 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        a=rx srp
        j-a
        ; ======== function test_HairpinEnable ========
        ; frame-size: 164, params: 1, max-reg: 4, leaf: yes, vreg: yes, spills: 41
TEST_HAIRPINENABLE:
        push-r r4
        a=rx sp
        rx= -164 r0
        a+=rx r0
        rx=a sp
        ; 12429: wr_HairpinEnable_allowFlood(0, 0);
        ; 12430: wr_HairpinEnable_allowMc(0, 0);
        ; -- inline begin: result in __inline_result_i2413 --
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        ; 1743: int entry;
        a= 0
        a+=rx r2
        rx=a r2
        ; 1744: entry = readFromDevice(address,0);
        ; 1745: entry = (entry & ~(int)0x1) | ((int)(allowFlood & 0x1));
        rx= 65536 r3
        a+=rx r3
        rx=m[a] r3
        ; 1746: writeToDevice(address,entry,0);
        a= -2
        a&=rx r3
        rx=a r4
        a= 0
        a|=rx r4
        rx=a r3
        rx= 65536 r4
        a=rx r2
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2415:
        ; -- inline end --
INLINE_RET_i2413:
        ; -- inline end --
        ; 12431: wr_HairpinEnable_allowUc(0, 1);
        ; -- inline begin: result in __inline_result_i2416 --
        ; 1758: address = 56021;
        ; 1759: address += idx*1;
        rx= 56021 r2
        a=rx sp
        m[a+n]=rx 156 r2
        rx=m[a+n] 156 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 156 r2
        ; 1761: entry = readFromDevice(address,0);
        ; 1762: entry = (entry & ~((int)0x1 << 1)) | ((int)(allowMc & 0x1) << 1);
        rx= 65536 r2
        rx=m[a+n] 156 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 152 r2
        rx=m[a+n] 152 r2
        a= -3
        a&=rx r2
        rx=a r2
        a= 0
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 152 r2
        rx=m[a+n] 152 r2
        rx= 65536 r3
        rx=m[a+n] 156 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2418:
        a=rx sp
        rx=m[a+n] 152 r2
        ; -- inline end --
INLINE_RET_i2416:
        a=rx sp
        rx=m[a+n] 160 r2
        ; -- inline end --
        ; 12432: rd_HairpinEnable_allowFlood(0, &val);
        ; -- inline begin: result in __inline_result_i2419 --
        ; 1775: address = 56021;
        ; 1776: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 148 r2
        rx=m[a+n] 148 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 148 r2
        ; 1778: entry = readFromDevice(address,0);
        ; 1779: entry = (entry & ~((int)0x1 << 2)) | ((int)(allowUc & 0x1) << 2);
        rx= 65536 r2
        rx=m[a+n] 148 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 144 r2
        rx=m[a+n] 144 r2
        a= -5
        a&=rx r2
        rx=a r2
        a= 4
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 144 r2
        rx=m[a+n] 144 r2
        rx= 65536 r3
        rx=m[a+n] 148 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2421:
        a=rx sp
        rx=m[a+n] 144 r2
        ; -- inline end --
INLINE_RET_i2419:
        a=rx sp
        rx=m[a+n] 152 r2
        ; -- inline end --
        ; 12433: rd_HairpinEnable_allowMc(0, &val);
        ; -- inline begin: result in __inline_result_i2422 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 140 r2
        ; 1733: address = 56021;
        ; 1734: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 136 r2
        rx=m[a+n] 136 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 136 r2
        ; 1736: entry = readFromDevice(address,0);
        ; 1737: *out = (int)((entry) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 136 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 132 r2
        rx=m[a+n] 132 r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 140 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2422:
        a=rx sp
        rx=m[a+n] 140 r2
        ; -- inline end --
        ; 12434: rd_HairpinEnable_allowUc(0, &val);
        ; -- inline begin: result in __inline_result_i2424 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 128 r2
        ; 1750: address = 56021;
        ; 1751: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 124 r2
        rx=m[a+n] 124 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 124 r2
        ; 1753: entry = readFromDevice(address,0);
        ; 1754: *out = (int)((entry >> 1) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 124 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 120 r2
        rx=m[a+n] 120 r2
        a=rx r2
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 128 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2424:
        a=rx sp
        rx=m[a+n] 128 r2
        ; -- inline end --
        ; 12435: wr_HairpinEnable_allowFlood(0, 1);
        ; -- inline begin: result in __inline_result_i2426 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 116 r2
        ; 1767: address = 56021;
        ; 1768: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 112 r2
        rx=m[a+n] 112 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 112 r2
        ; 1770: entry = readFromDevice(address,0);
        ; 1771: *out = (int)((entry >> 2) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 112 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 108 r2
        rx=m[a+n] 108 r2
        a=rx r2
        a=a>>1
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 116 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2426:
        a=rx sp
        rx=m[a+n] 116 r2
        ; -- inline end --
        ; 12436: rd_HairpinEnable_allowFlood(0, &val);
        ; -- inline begin: result in __inline_result_i2428 --
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 104 r2
        rx=m[a+n] 104 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 104 r2
        ; 1744: entry = readFromDevice(address,0);
        ; 1745: entry = (entry & ~(int)0x1) | ((int)(allowFlood & 0x1));
        rx= 65536 r2
        rx=m[a+n] 104 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 100 r2
        rx=m[a+n] 100 r2
        a= -2
        a&=rx r2
        rx=a r2
        a= 1
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 100 r2
        rx=m[a+n] 100 r2
        rx= 65536 r3
        rx=m[a+n] 104 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2430:
        a=rx sp
        rx=m[a+n] 100 r2
        ; -- inline end --
INLINE_RET_i2428:
        a=rx sp
        rx=m[a+n] 108 r2
        ; -- inline end --
        ; 12437: rd_HairpinEnable_allowMc(0, &val);
        ; -- inline begin: result in __inline_result_i2431 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 96 r2
        ; 1733: address = 56021;
        ; 1734: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 92 r2
        rx=m[a+n] 92 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 92 r2
        ; 1736: entry = readFromDevice(address,0);
        ; 1737: *out = (int)((entry) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 92 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 88 r2
        rx=m[a+n] 88 r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 96 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2431:
        a=rx sp
        rx=m[a+n] 96 r2
        ; -- inline end --
        ; 12438: rd_HairpinEnable_allowUc(0, &val);
        ; -- inline begin: result in __inline_result_i2433 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 84 r2
        ; 1750: address = 56021;
        ; 1751: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 80 r2
        rx=m[a+n] 80 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 80 r2
        ; 1753: entry = readFromDevice(address,0);
        ; 1754: *out = (int)((entry >> 1) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 80 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 76 r2
        rx=m[a+n] 76 r2
        a=rx r2
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 84 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2433:
        a=rx sp
        rx=m[a+n] 84 r2
        ; -- inline end --
        ; 12439: wr_HairpinEnable_allowFlood(1, 0);
        ; -- inline begin: result in __inline_result_i2435 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 72 r2
        ; 1767: address = 56021;
        ; 1768: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 68 r2
        rx=m[a+n] 68 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 68 r2
        ; 1770: entry = readFromDevice(address,0);
        ; 1771: *out = (int)((entry >> 2) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 68 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 64 r2
        rx=m[a+n] 64 r2
        a=rx r2
        a=a>>1
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 72 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2435:
        a=rx sp
        rx=m[a+n] 72 r2
        ; -- inline end --
        ; 12440: wr_HairpinEnable_allowMc(1, 0);
        ; -- inline begin: result in __inline_result_i2437 --
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 60 r2
        rx=m[a+n] 60 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 60 r2
        ; 1744: entry = readFromDevice(address,0);
        ; 1745: entry = (entry & ~(int)0x1) | ((int)(allowFlood & 0x1));
        rx= 65536 r2
        rx=m[a+n] 60 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 56 r2
        rx=m[a+n] 56 r2
        a= -2
        a&=rx r2
        rx=a r2
        a= 0
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 56 r2
        rx=m[a+n] 56 r2
        rx= 65536 r3
        rx=m[a+n] 60 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2439:
        a=rx sp
        rx=m[a+n] 56 r2
        ; -- inline end --
INLINE_RET_i2437:
        a=rx sp
        rx=m[a+n] 64 r2
        ; -- inline end --
        ; 12441: wr_HairpinEnable_allowUc(1, 1);
        ; -- inline begin: result in __inline_result_i2440 --
        ; 1758: address = 56021;
        ; 1759: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 52 r2
        rx=m[a+n] 52 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 52 r2
        ; 1761: entry = readFromDevice(address,0);
        ; 1762: entry = (entry & ~((int)0x1 << 1)) | ((int)(allowMc & 0x1) << 1);
        rx= 65536 r2
        rx=m[a+n] 52 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 48 r2
        rx=m[a+n] 48 r2
        a= -3
        a&=rx r2
        rx=a r2
        a= 0
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 48 r2
        rx=m[a+n] 48 r2
        rx= 65536 r3
        rx=m[a+n] 52 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2442:
        a=rx sp
        rx=m[a+n] 48 r2
        ; -- inline end --
INLINE_RET_i2440:
        a=rx sp
        rx=m[a+n] 56 r2
        ; -- inline end --
        ; 12442: wr_HairpinEnable_allowFlood(0, 0);
        ; -- inline begin: result in __inline_result_i2443 --
        ; 1775: address = 56021;
        ; 1776: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 44 r2
        rx=m[a+n] 44 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 44 r2
        ; 1778: entry = readFromDevice(address,0);
        ; 1779: entry = (entry & ~((int)0x1 << 2)) | ((int)(allowUc & 0x1) << 2);
        rx= 65536 r2
        rx=m[a+n] 44 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 40 r2
        rx=m[a+n] 40 r2
        a= -5
        a&=rx r2
        rx=a r2
        a= 4
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 40 r2
        rx=m[a+n] 40 r2
        rx= 65536 r3
        rx=m[a+n] 44 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2445:
        a=rx sp
        rx=m[a+n] 40 r2
        ; -- inline end --
INLINE_RET_i2443:
        a=rx sp
        rx=m[a+n] 48 r2
        ; -- inline end --
        ; 12443: rd_HairpinEnable_allowFlood(1, &val);
        ; -- inline begin: result in __inline_result_i2446 --
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 36 r2
        rx=m[a+n] 36 r2
        a= 0
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 36 r2
        ; 1744: entry = readFromDevice(address,0);
        ; 1745: entry = (entry & ~(int)0x1) | ((int)(allowFlood & 0x1));
        rx= 65536 r2
        rx=m[a+n] 36 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 32 r2
        rx=m[a+n] 32 r2
        a= -2
        a&=rx r2
        rx=a r2
        a= 0
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 32 r2
        rx=m[a+n] 32 r2
        rx= 65536 r3
        rx=m[a+n] 36 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2448:
        a=rx sp
        rx=m[a+n] 32 r2
        ; -- inline end --
INLINE_RET_i2446:
        a=rx sp
        rx=m[a+n] 40 r2
        ; -- inline end --
        ; 12444: rd_HairpinEnable_allowUc(1, &val);
        ; -- inline begin: result in __inline_result_i2449 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 28 r2
        ; 1733: address = 56021;
        ; 1734: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 24 r2
        rx=m[a+n] 24 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 24 r2
        ; 1736: entry = readFromDevice(address,0);
        ; 1737: *out = (int)((entry) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 24 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 28 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2449:
        a=rx sp
        rx=m[a+n] 28 r2
        ; -- inline end --
        ; 12445: }
        ; -- inline begin: result in __inline_result_i2451 --
        rx= 160 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 16 r2
        ; 1767: address = 56021;
        ; 1768: address += idx*1;
        rx= 56021 r2
        m[a+n]=rx 12 r2
        rx=m[a+n] 12 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 12 r2
        ; 1770: entry = readFromDevice(address,0);
        ; 1771: *out = (int)((entry >> 2) & 0x1);
        rx= 65536 r2
        rx=m[a+n] 12 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 8 r2
        rx=m[a+n] 8 r2
        a=rx r2
        a=a>>1
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 16 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2451:
        a=rx sp
        rx=m[a+n] 16 r2
        ; -- inline end --
TEST_HAIRPINENABLE_END53:
        a=rx sp
        rx= 164 r0
        a+=rx r0
        rx=a sp
        pop-r r4
        a=rx srp
        j-a
        ; ======== function test_EgressPortConfiguration_crossword ========
        ; frame-size: 212, params: 1, max-reg: 5, leaf: no, vreg: yes, spills: 53
TEST_EGRESSPORTCONFIGURATION_CROSSWORD:
        push-srp
        push-r r5
        a=rx sp
        rx= -216 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 212 p0
        ; 12448: int idx = 0;
        ; 12449: wr_EgressPortConfiguration_colorRemap(idx, 0);
        rx= 0 r2
        ; 12450: wr_EgressPortConfiguration_vid(idx, 0);
        ; -- inline begin: result in __inline_result_i2453 --
        ; 826: address = 58126;
        ; 827: address += idx*2;
        rx= 58126 r3
        ; 828: int entry;
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        ; 829: entry = readFromDevice(address,0);
        ; 830: entry = (entry & ~(int)0x1) | ((int)(colorRemap & 0x1));
        rx= 65536 r4
        a+=rx r4
        rx=m[a] r4
        ; 831: writeToDevice(address,entry,0);
        a= -2
        a&=rx r4
        rx=a r5
        a= 0
        a|=rx r5
        rx=a r4
        rx= 65536 r5
        a=rx r3
        a+=rx r5
        m[a]=rx r4
INLINE_RET_i2455:
        a=rx sp
        rx=m[a+n] 208 r3
        ; -- inline end --
INLINE_RET_i2453:
        ; -- inline end --
        ; 12451: wr_EgressPortConfiguration_moreThanOneVlans(idx, 0);
        ; -- inline begin: result in __inline_result_i2456 --
        ; 928: address = 58126;
        ; 929: address += idx*2;
        rx= 58126 r3
        a=rx sp
        m[a+n]=rx 204 r3
        rx=m[a+n] 204 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 204 r3
        ; 931: entry = readFromDevice(address,0);
        ; 932: entry = (entry & ~((int)0xfff << 12)) | ((int)(vid & 0xfff) << 12);
        rx= 65536 r3
        rx=m[a+n] 204 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 200 r3
        rx=m[a+n] 200 r3
        rx= -16773121 r4
        a=rx r4
        a&=rx r3
        rx=a r3
        a= 0
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 200 r3
        rx=m[a+n] 200 r3
        rx= 65536 r4
        rx=m[a+n] 204 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2458:
        a=rx sp
        rx=m[a+n] 200 r3
        ; -- inline end --
INLINE_RET_i2456:
        a=rx sp
        rx=m[a+n] 208 r3
        ; -- inline end --
        ; 12452: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 0);
        ; -- inline begin: result in __inline_result_i2459 --
        ; 1030: address = 58126;
        ; 1031: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 196 r3
        rx=m[a+n] 196 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 196 r3
        ; 1033: entry = readFromDevice(address,0);
        ; 1034: entry = (entry & ~((int)0x1 << 31)) | ((int)(moreThanOneVlans & 0x1) << 31);
        rx= 65536 r3
        rx=m[a+n] 196 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 192 r3
        rx=m[a+n] 192 r3
        rx= 2147483647 r4
        a=rx r4
        a&=rx r3
        rx=a r3
        a= 0
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 192 r3
        rx=m[a+n] 192 r3
        rx= 65536 r4
        rx=m[a+n] 196 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2461:
        a=rx sp
        rx=m[a+n] 192 r3
        ; -- inline end --
INLINE_RET_i2459:
        a=rx sp
        rx=m[a+n] 200 r3
        ; -- inline end --
        ; 12453: wr_EgressPortConfiguration_dropSingleTaggedVlans(idx, 0);
        ; -- inline begin: result in __inline_result_i2462 --
        ; 1047: address = 58126;
        ; 1048: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 188 r3
        rx=m[a+n] 188 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 188 r3
        rx=m[a+n] 188 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 180 r3
        rx= 65536 r3
        rx=m[a+n] 180 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 176 r3
INLINE_RET_i2463:
        a=rx sp
        rx=m[a+n] 176 r3
        m[a+n]=rx 184 r3
        rx=m[a+n] 184 r3
        a= -2
        a&=rx r3
        rx=a r3
        a= 0
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 184 r3
        rx=m[a+n] 188 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 172 r3
        rx=m[a+n] 184 r3
        rx= 65536 r4
        rx=m[a+n] 172 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2464:
        a=rx sp
        rx=m[a+n] 172 r3
        ; -- inline end --
INLINE_RET_i2462:
        a=rx sp
        rx=m[a+n] 192 r3
        ; -- inline end --
        ; 12454: wr_EgressPortConfiguration_moreThanOneVlans(idx, 1);
        ; -- inline begin: result in __inline_result_i2465 --
        ; 1064: address = 58126;
        ; 1065: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 168 r3
        rx=m[a+n] 168 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 168 r3
        rx=m[a+n] 168 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 160 r3
        rx= 65536 r3
        rx=m[a+n] 160 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 156 r3
INLINE_RET_i2466:
        a=rx sp
        rx=m[a+n] 156 r3
        m[a+n]=rx 164 r3
        rx=m[a+n] 164 r3
        a= -3
        a&=rx r3
        rx=a r3
        a= 0
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 164 r3
        rx=m[a+n] 168 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 152 r3
        rx=m[a+n] 164 r3
        rx= 65536 r4
        rx=m[a+n] 152 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2467:
        a=rx sp
        rx=m[a+n] 152 r3
        ; -- inline end --
INLINE_RET_i2465:
        a=rx sp
        rx=m[a+n] 172 r3
        ; -- inline end --
        ; 12455: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2468 --
        ; 1030: address = 58126;
        ; 1031: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 148 r3
        rx=m[a+n] 148 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 148 r3
        ; 1033: entry = readFromDevice(address,0);
        ; 1034: entry = (entry & ~((int)0x1 << 31)) | ((int)(moreThanOneVlans & 0x1) << 31);
        rx= 65536 r3
        rx=m[a+n] 148 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 144 r3
        rx=m[a+n] 144 r3
        rx= 2147483647 r4
        a=rx r4
        a&=rx r3
        rx=a r3
        rx= -2147483648 r4
        a=rx r4
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 144 r3
        rx=m[a+n] 144 r3
        rx= 65536 r4
        rx=m[a+n] 148 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2470:
        a=rx sp
        rx=m[a+n] 144 r3
        ; -- inline end --
INLINE_RET_i2468:
        a=rx sp
        rx=m[a+n] 152 r3
        ; -- inline end --
        ; 12456: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2471 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 140 r3
        ; 1022: address = 58126;
        ; 1023: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 136 r3
        rx=m[a+n] 136 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 136 r3
        ; 1025: entry = readFromDevice(address,0);
        ; 1026: *out = (int)((entry >> 31) & 0x1);
        rx= 65536 r3
        rx=m[a+n] 136 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 132 r3
        rx=m[a+n] 132 r3
        a=rx r3
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r3
        a= 1
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 140 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2471:
        a=rx sp
        rx=m[a+n] 140 r3
        ; -- inline end --
        ; 12457: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 1);
        ; -- inline begin: result in __inline_result_i2473 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 128 r3
        ; 1039: address = 58126;
        ; 1040: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 124 r3
        rx=m[a+n] 124 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 124 r3
        rx=m[a+n] 124 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 116 r3
        rx= 65536 r3
        rx=m[a+n] 116 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 112 r3
INLINE_RET_i2474:
        a=rx sp
        rx=m[a+n] 112 r3
        m[a+n]=rx 120 r3
        rx=m[a+n] 120 r3
        a= 1
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 128 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2473:
        a=rx sp
        rx=m[a+n] 128 r3
        ; -- inline end --
        ; 12458: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2475 --
        ; 1047: address = 58126;
        ; 1048: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 108 r3
        rx=m[a+n] 108 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 108 r3
        rx=m[a+n] 108 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 100 r3
        rx= 65536 r3
        rx=m[a+n] 100 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 96 r3
INLINE_RET_i2476:
        a=rx sp
        rx=m[a+n] 96 r3
        m[a+n]=rx 104 r3
        rx=m[a+n] 104 r3
        a= -2
        a&=rx r3
        rx=a r3
        a= 1
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 104 r3
        rx=m[a+n] 108 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 92 r3
        rx=m[a+n] 104 r3
        rx= 65536 r4
        rx=m[a+n] 92 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2477:
        a=rx sp
        rx=m[a+n] 92 r3
        ; -- inline end --
INLINE_RET_i2475:
        a=rx sp
        rx=m[a+n] 112 r3
        ; -- inline end --
        ; 12459: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2478 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 88 r3
        ; 1039: address = 58126;
        ; 1040: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 84 r3
        rx=m[a+n] 84 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 84 r3
        rx=m[a+n] 84 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 76 r3
        rx= 65536 r3
        rx=m[a+n] 76 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 72 r3
INLINE_RET_i2479:
        a=rx sp
        rx=m[a+n] 72 r3
        m[a+n]=rx 80 r3
        rx=m[a+n] 80 r3
        a= 1
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 88 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2478:
        a=rx sp
        rx=m[a+n] 88 r3
        ; -- inline end --
        ; 12460: wr_EgressPortConfiguration_vid(idx, 0xabc);
        ; -- inline begin: result in __inline_result_i2480 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 68 r3
        ; 1022: address = 58126;
        ; 1023: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 64 r3
        rx=m[a+n] 64 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 64 r3
        ; 1025: entry = readFromDevice(address,0);
        ; 1026: *out = (int)((entry >> 31) & 0x1);
        rx= 65536 r3
        rx=m[a+n] 64 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 60 r3
        rx=m[a+n] 60 r3
        a=rx r3
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r3
        a= 1
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 68 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2480:
        a=rx sp
        rx=m[a+n] 68 r3
        ; -- inline end --
        ; 12461: rd_EgressPortConfiguration_vid(idx, &val);
        ; -- inline begin: result in __inline_result_i2482 --
        ; 928: address = 58126;
        ; 929: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 56 r3
        rx=m[a+n] 56 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 56 r3
        ; 931: entry = readFromDevice(address,0);
        ; 932: entry = (entry & ~((int)0xfff << 12)) | ((int)(vid & 0xfff) << 12);
        rx= 65536 r3
        rx=m[a+n] 56 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 52 r3
        rx=m[a+n] 52 r3
        rx= -16773121 r4
        a=rx r4
        a&=rx r3
        rx=a r3
        rx= 11255808 r4
        a=rx r4
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 52 r3
        rx=m[a+n] 52 r3
        rx= 65536 r4
        rx=m[a+n] 56 r5
        a=rx r5
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2484:
        a=rx sp
        rx=m[a+n] 52 r3
        ; -- inline end --
INLINE_RET_i2482:
        a=rx sp
        rx=m[a+n] 60 r3
        ; -- inline end --
        ; 12462: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2485 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 48 r3
        ; 920: address = 58126;
        ; 921: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 44 r3
        rx=m[a+n] 44 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 44 r3
        ; 923: entry = readFromDevice(address,0);
        ; 924: *out = (int)((entry >> 12) & 0xfff);
        rx= 65536 r3
        rx=m[a+n] 44 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 40 r3
        rx=m[a+n] 40 r3
        a=rx r3
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r3
        rx= 4095 r4
        a=rx r4
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 48 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2485:
        a=rx sp
        rx=m[a+n] 48 r3
        ; -- inline end --
        ; 12463: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i2487 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 36 r3
        ; 1022: address = 58126;
        ; 1023: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 32 r3
        rx=m[a+n] 32 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 32 r3
        ; 1025: entry = readFromDevice(address,0);
        ; 1026: *out = (int)((entry >> 31) & 0x1);
        rx= 65536 r3
        rx=m[a+n] 32 r4
        a=rx r4
        a+=rx r3
        rx=m[a] r3
        a=rx sp
        m[a+n]=rx 28 r3
        rx=m[a+n] 28 r3
        a=rx r3
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r3
        a= 1
        a&=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 36 r4
        a=rx r4
        m[a]=rx r3
INLINE_RET_i2487:
        a=rx sp
        rx=m[a+n] 36 r3
        ; -- inline end --
        ; 12464: }
        ; -- inline begin: result in __inline_result_i2489 --
        rx= 208 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 24 r3
        ; 1039: address = 58126;
        ; 1040: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 20 r3
        rx=m[a+n] 20 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        a= 1
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 12 r2
        rx= 65536 r2
        rx=m[a+n] 12 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 8 r2
INLINE_RET_i2490:
        a=rx sp
        rx=m[a+n] 8 r2
        m[a+n]=rx 16 r2
        rx=m[a+n] 16 r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 24 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2489:
        a=rx sp
        rx=m[a+n] 24 r2
        ; -- inline end --
TEST_EGRESSPORTCONFIGURATION_CROSSWORD_END54:
        a=rx sp
        rx= 216 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function test_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 212, params: 1, max-reg: 9, leaf: no, vreg: yes, spills: 53
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -216 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 212 p0
        ; 12494: unsigned char out[16];
        rx= 0 r6
        ; 12495: unsigned char in[16];
        ; 12496: unsigned char zeros[16] = {0};
        ; 12498: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        ; -- inline begin: result in __inline_result_i2529 --
        rx= 16 r0
        m[a+n]=rx 112 r0
        rx= 180 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 104 r0
        ; 12369: return s;
WHILE79:
        a=rx sp
        rx=m[a+n] 112 r0
        a=rx r0
        rx= -1 r1
        a+=rx r1
        rx=a r1
        a=rx sp
        m[a+n]=rx 112 r1
        a=rx r0
        rx= 0 r0
        a-=rx r0
        jz endwhile80
        ; 12369: return s;
        rx= 0 r0
        a=rx sp
        rx=m[a+n] 104 r1
        a=rx r1
        rx= 1 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 104 r2
        a=rx r1
        m[a].b=rx r0
        j while79
ENDWHILE80:
        a=rx sp
        rx= 180 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 108 r0
INLINE_RET_i2529:
        a=rx sp
        rx=m[a+n] 108 r0
        ; -- inline end --
        ; 12499: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12500: for (int i = 0; i < 16; i++) in[i] = (unsigned char)(i + 1);
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 0 r9
FOR83:
        rx= 16 r0
        a=rx r9
        a-=rx r0
        jge endfor85
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 1
        a+=rx r9
        mask-a-b
        rx=a r0
        a=rx sp
        rx= 180 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT84:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for83
ENDFOR85:
        ; 12502: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12503: memset(in, 0xff, 16);
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12504: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        ; -- inline begin: result in __inline_result_i2530 --
        rx= 16 r0
        a=rx sp
        m[a+n]=rx 100 r0
        rx= 180 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 92 r0
        ; 12369: return s;
WHILE86:
        a=rx sp
        rx=m[a+n] 100 r0
        a=rx r0
        rx= -1 r1
        a+=rx r1
        rx=a r1
        a=rx sp
        m[a+n]=rx 100 r1
        a=rx r0
        rx= 0 r0
        a-=rx r0
        jz endwhile87
        ; 12369: return s;
        rx= 255 r0
        a=rx sp
        rx=m[a+n] 92 r1
        a=rx r1
        rx= 1 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 92 r2
        a=rx r1
        m[a].b=rx r0
        j while86
ENDWHILE87:
        a=rx sp
        rx= 180 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 96 r0
INLINE_RET_i2530:
        a=rx sp
        rx=m[a+n] 96 r0
        ; -- inline end --
        ; 12505: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12506: for (int i = 0; i < 16; i++) in[i] = (i & 1) ? 0x55 : 0xaa;
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12507: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 0 r9
FOR90:
        rx= 16 r0
        a=rx r9
        a-=rx r0
        jge endfor92
        ; 12507: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 1
        a&=rx r9
        rx= 0 r0
        a-=rx r0
        jz ternelse93
        rx= 85 r0
        a=rx r0
        j ternend94
TERNELSE93:
        rx= 170 r0
        a=rx r0
TERNEND94:
        rx=a r0
        a=rx sp
        rx= 180 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT91:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for90
ENDFOR92:
        ; 12508: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12509: wr_HashBasedL3RoutingTable_proto(idx, 2);
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12510: wr_HashBasedL3RoutingTable_vrf(idx, 1);
        ; -- inline begin: result in __inline_result_i2531 --
        ; 1877: address = 12887;
        ; 1878: address += idx*8;
        rx= 12887 r0
        a=rx sp
        m[a+n]=rx 88 r0
        rx=m[a+n] 88 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 88 r0
        ; 1880: entry = readFromDevice(address,0);
        ; 1881: entry = (entry & ~(int)0x3) | ((int)(proto & 0x3));
        rx= 65536 r0
        rx=m[a+n] 88 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 84 r0
        rx=m[a+n] 84 r0
        a= -4
        a&=rx r0
        rx=a r0
        a= 2
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 84 r0
        rx=m[a+n] 84 r0
        rx= 65536 r1
        rx=m[a+n] 88 r2
        a=rx r2
        a+=rx r1
        m[a]=rx r0
INLINE_RET_i2533:
        a=rx sp
        rx=m[a+n] 84 r0
        ; -- inline end --
INLINE_RET_i2531:
        a=rx sp
        rx=m[a+n] 92 r0
        ; -- inline end --
        ; 12511: for (int i = 0; i < 16; i++) in[i] = (unsigned char)(0x10 + i);
        ; -- inline begin: result in __inline_result_i2534 --
        ; 1894: address = 12887;
        ; 1895: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 80 r0
        rx=m[a+n] 80 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 80 r0
        ; 1897: entry = readFromDevice(address,0);
        ; 1898: entry = (entry & ~((int)0x3 << 2)) | ((int)(vrf & 0x3) << 2);
        rx= 65536 r0
        rx=m[a+n] 80 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 76 r0
        rx=m[a+n] 76 r0
        rx= -13 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a= 4
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 76 r0
        rx=m[a+n] 76 r0
        rx= 65536 r1
        rx=m[a+n] 80 r2
        a=rx r2
        a+=rx r1
        m[a]=rx r0
INLINE_RET_i2536:
        a=rx sp
        rx=m[a+n] 76 r0
        ; -- inline end --
INLINE_RET_i2534:
        a=rx sp
        rx=m[a+n] 84 r0
        ; -- inline end --
        ; 12512: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 0 r9
FOR95:
        rx= 16 r0
        a=rx r9
        a-=rx r0
        jge endfor97
        ; 12512: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 16 r0
        a=rx r9
        a+=rx r0
        mask-a-b
        rx=a r0
        a=rx sp
        rx= 180 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT96:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for95
ENDFOR97:
        ; 12513: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12514: int proto_val, vrf_val;
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12515: rd_HashBasedL3RoutingTable_proto(idx, &proto_val);
        ; 12516: rd_HashBasedL3RoutingTable_vrf(idx, &vrf_val);
        ; -- inline begin: result in __inline_result_i2537 --
        a=rx sp
        rx= 160 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 72 r0
        ; 1869: address = 12887;
        ; 1870: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 68 r0
        rx=m[a+n] 68 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 68 r0
        ; 1872: entry = readFromDevice(address,0);
        ; 1873: *out = (int)((entry) & 0x3);
        rx= 65536 r0
        rx=m[a+n] 68 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 64 r0
        rx=m[a+n] 64 r0
        a= 3
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 72 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i2537:
        a=rx sp
        rx=m[a+n] 72 r0
        ; -- inline end --
        ; 12517: wr_HashBasedL3RoutingTable_nextHopPointer(idx, 0x1aa);
        ; -- inline begin: result in __inline_result_i2539 --
        rx= 156 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 60 r0
        ; 1886: address = 12887;
        ; 1887: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 56 r0
        rx=m[a+n] 56 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 56 r0
        ; 1889: entry = readFromDevice(address,0);
        ; 1890: *out = (int)((entry >> 2) & 0x3);
        rx= 65536 r0
        rx=m[a+n] 56 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 52 r0
        rx=m[a+n] 52 r0
        a=rx r0
        a=a>>1
        a=a>>1
        rx=a r0
        a= 3
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 60 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i2539:
        a=rx sp
        rx=m[a+n] 60 r0
        ; -- inline end --
        ; 12518: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        ; -- inline begin: result in __inline_result_i2541 --
        ; 1945: address = 12887;
        ; 1946: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 48 r0
        rx=m[a+n] 48 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 48 r0
        rx=m[a+n] 48 r0
        a= 4
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 40 r0
        rx= 65536 r0
        rx=m[a+n] 40 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 36 r0
INLINE_RET_i2542:
        a=rx sp
        rx=m[a+n] 36 r0
        m[a+n]=rx 44 r0
        rx=m[a+n] 44 r0
        rx= -8177 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        rx= 6816 r1
        a=rx r1
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 44 r0
        rx=m[a+n] 48 r0
        a= 4
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 32 r0
        rx=m[a+n] 44 r0
        rx= 65536 r1
        rx=m[a+n] 32 r2
        a=rx r2
        a+=rx r1
        m[a]=rx r0
INLINE_RET_i2543:
        a=rx sp
        rx=m[a+n] 32 r0
        ; -- inline end --
INLINE_RET_i2541:
        a=rx sp
        rx=m[a+n] 52 r0
        ; -- inline end --
        ; 12519: int nhp;
        rx= 0 r0
        rx= 180 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12520: rd_HashBasedL3RoutingTable_nextHopPointer(idx, &nhp);
        ; 12521: int idx1 = 1;
        ; -- inline begin: result in __inline_result_i2544 --
        a=rx sp
        rx= 152 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 28 r0
        ; 1937: address = 12887;
        ; 1938: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 24 r0
        rx=m[a+n] 24 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 24 r0
        rx=m[a+n] 24 r0
        a= 4
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 16 r0
        rx= 65536 r0
        rx=m[a+n] 16 r1
        a=rx r1
        a+=rx r0
        rx=m[a] r0
        a=rx sp
        m[a+n]=rx 12 r0
INLINE_RET_i2545:
        a=rx sp
        rx=m[a+n] 12 r0
        m[a+n]=rx 20 r0
        rx=m[a+n] 20 r0
        a=rx r0
        a=a>>1
        a=a>>1
        a=a>>1
        a=a>>1
        rx=a r0
        rx= 511 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 28 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i2544:
        a=rx sp
        rx=m[a+n] 28 r0
        ; -- inline end --
        ; 12523: memset(in1, 0xcc, 16);
        ; 12524: wr_HashBasedL3RoutingTable_destIPAddr(idx1, in1);
        ; -- inline begin: result in __inline_result_i2546 --
        rx= 16 r0
        m[a+n]=rx 8 r0
        rx= 132 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        ; 12369: return s;
WHILE98:
        a=rx sp
        rx=m[a+n] 8 r0
        a=rx r0
        rx= -1 r1
        a+=rx r1
        rx=a r1
        a=rx sp
        m[a+n]=rx 8 r1
        a=rx r0
        rx= 0 r0
        a-=rx r0
        jz endwhile99
        ; 12369: return s;
        rx= 204 r0
        a=rx sp
        rx=m[a+n] 0 r1
        a=rx r1
        rx= 1 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 0 r2
        a=rx r1
        m[a].b=rx r0
        j while98
ENDWHILE99:
        a=rx sp
        rx= 132 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
INLINE_RET_i2546:
        a=rx sp
        rx=m[a+n] 4 r0
        ; -- inline end --
        ; 12525: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 1 r0
        rx= 132 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12526: rd_HashBasedL3RoutingTable_destIPAddr(idx1, out1);
        rx= 0 r0
        a=rx sp
        rx= 196 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12527: }
        rx= 1 r0
        a=rx sp
        rx= 116 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END55:
        a=rx sp
        rx= 216 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        ; ======== function main ========
        ; frame-size: 168, params: 1, max-reg: 4, leaf: no, vreg: yes, spills: 42
MAIN:
        push-srp
        push-r r4
        a=rx sp
        rx= -172 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 168 p0
        ; 12530: test_CoreTickSelect();
        jsr test_coretickconfiguration
        ; 12531: test_ERMRedConfiguration();
        ; -- inline begin: result in __inline_result_i2547 --
        ; 12397: wr_CoreTickSelect_clkSelect(1);
        ; 12398: rd_CoreTickSelect_clkSelect(&val);
        ; -- inline begin: result in __inline_result_i2548 --
        ; 299: address = 3;
        ; 300: int entry;
        rx= 3 r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        rx= 65536 r3
        a=rx r2
        a+=rx r3
        rx=m[a] r3
        ; 303: writeToDevice(address,entry,0);
        a= -4
        a&=rx r3
        rx=a r4
        a= 1
        a|=rx r4
        rx=a r3
        rx= 65536 r4
        a=rx r2
        a+=rx r4
        m[a]=rx r3
INLINE_RET_i2550:
        a=rx sp
        rx=m[a+n] 164 r2
        ; -- inline end --
INLINE_RET_i2548:
        ; -- inline end --
        ; 12399: wr_CoreTickSelect_clkSelect(2);
        ; -- inline begin: result in __inline_result_i2551 --
        a=rx sp
        rx= 164 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 160 r2
        ; 292: address = 3;
        ; 293: int entry;
        rx= 3 r2
        m[a+n]=rx 156 r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        rx= 65536 r2
        rx=m[a+n] 156 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 152 r2
        rx=m[a+n] 152 r2
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 160 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2551:
        a=rx sp
        rx=m[a+n] 160 r2
        ; -- inline end --
        ; 12400: rd_CoreTickSelect_clkSelect(&val);
        ; -- inline begin: result in __inline_result_i2553 --
        ; 299: address = 3;
        ; 300: int entry;
        rx= 3 r2
        m[a+n]=rx 148 r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        rx= 65536 r2
        rx=m[a+n] 148 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 144 r2
        rx=m[a+n] 144 r2
        a= -4
        a&=rx r2
        rx=a r2
        a= 2
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 144 r2
        rx=m[a+n] 144 r2
        rx= 65536 r3
        rx=m[a+n] 148 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2555:
        a=rx sp
        rx=m[a+n] 144 r2
        ; -- inline end --
INLINE_RET_i2553:
        a=rx sp
        rx=m[a+n] 152 r2
        ; -- inline end --
        ; 12401: wr_CoreTickSelect_clkSelect(3);
        ; -- inline begin: result in __inline_result_i2556 --
        rx= 164 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 140 r2
        ; 292: address = 3;
        ; 293: int entry;
        rx= 3 r2
        m[a+n]=rx 136 r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        rx= 65536 r2
        rx=m[a+n] 136 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 132 r2
        rx=m[a+n] 132 r2
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 140 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2556:
        a=rx sp
        rx=m[a+n] 140 r2
        ; -- inline end --
        ; 12402: rd_CoreTickSelect_clkSelect(&val);
        ; -- inline begin: result in __inline_result_i2558 --
        ; 299: address = 3;
        ; 300: int entry;
        rx= 3 r2
        m[a+n]=rx 128 r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        rx= 65536 r2
        rx=m[a+n] 128 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 124 r2
        rx=m[a+n] 124 r2
        a= -4
        a&=rx r2
        rx=a r2
        a= 3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 124 r2
        rx=m[a+n] 124 r2
        rx= 65536 r3
        rx=m[a+n] 128 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2560:
        a=rx sp
        rx=m[a+n] 124 r2
        ; -- inline end --
INLINE_RET_i2558:
        a=rx sp
        rx=m[a+n] 132 r2
        ; -- inline end --
        ; 12403: wr_CoreTickSelect_clkSelect(0xff);
        ; -- inline begin: result in __inline_result_i2561 --
        rx= 164 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 120 r2
        ; 292: address = 3;
        ; 293: int entry;
        rx= 3 r2
        m[a+n]=rx 116 r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        rx= 65536 r2
        rx=m[a+n] 116 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 112 r2
        rx=m[a+n] 112 r2
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 120 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2561:
        a=rx sp
        rx=m[a+n] 120 r2
        ; -- inline end --
        ; 12404: rd_CoreTickSelect_clkSelect(&val);
        ; -- inline begin: result in __inline_result_i2563 --
        ; 299: address = 3;
        ; 300: int entry;
        rx= 3 r2
        m[a+n]=rx 108 r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        rx= 65536 r2
        rx=m[a+n] 108 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 104 r2
        rx=m[a+n] 104 r2
        a= -4
        a&=rx r2
        rx=a r2
        a= 3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 104 r2
        rx=m[a+n] 104 r2
        rx= 65536 r3
        rx=m[a+n] 108 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2565:
        a=rx sp
        rx=m[a+n] 104 r2
        ; -- inline end --
INLINE_RET_i2563:
        a=rx sp
        rx=m[a+n] 112 r2
        ; -- inline end --
        ; 12405: }
        ; -- inline begin: result in __inline_result_i2566 --
        rx= 164 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 100 r2
        ; 292: address = 3;
        ; 293: int entry;
        rx= 3 r2
        m[a+n]=rx 96 r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        rx= 65536 r2
        rx=m[a+n] 96 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 92 r2
        rx=m[a+n] 92 r2
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 100 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2566:
        a=rx sp
        rx=m[a+n] 100 r2
        ; -- inline end --
INLINE_RET_i2547:
        a=rx r2
        ; -- inline end --
        ; 12532: test_HairpinEnable();
        jsr test_ermredconfiguration
        a=rx p0
        ; 12533: test_EgressPortConfiguration_crossword();
        jsr test_hairpinenable
        a=rx p0
        ; 12534: test_EgressEthernetTypeforVLANtag();
        jsr test_egressportconfiguration_crossword
        ; 12535: test_HashBasedL3RoutingTable_destIPAddr();
        ; -- inline begin: result in __inline_result_i2568 --
        ; 12467: wr_EgressEthernetTypeforVLANtag_typeValue(0xffff);
        ; 12468: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        ; -- inline begin: result in __inline_result_i2569 --
        ; 727: address = 61614;
        ; 728: int entry;
        rx= 61614 r2
        a=rx sp
        m[a+n]=rx 84 r2
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        rx= 65536 r2
        rx=m[a+n] 84 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 80 r2
        rx=m[a+n] 80 r2
        rx= -65536 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 65535 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 80 r2
        rx=m[a+n] 80 r2
        rx= 65536 r3
        rx=m[a+n] 84 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2571:
        a=rx sp
        rx=m[a+n] 80 r2
        ; -- inline end --
INLINE_RET_i2569:
        a=rx sp
        rx=m[a+n] 88 r2
        ; -- inline end --
        ; 12469: wr_EgressEthernetTypeforVLANtag_typeValue(0x8100);
        ; -- inline begin: result in __inline_result_i2572 --
        rx= 88 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 76 r2
        ; 720: address = 61614;
        ; 721: int entry;
        rx= 61614 r2
        m[a+n]=rx 72 r2
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        rx= 65536 r2
        rx=m[a+n] 72 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 68 r2
        rx=m[a+n] 68 r2
        rx= 65535 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 76 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2572:
        a=rx sp
        rx=m[a+n] 76 r2
        ; -- inline end --
        ; 12470: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        ; -- inline begin: result in __inline_result_i2574 --
        ; 727: address = 61614;
        ; 728: int entry;
        rx= 61614 r2
        m[a+n]=rx 64 r2
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        rx= 65536 r2
        rx=m[a+n] 64 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 60 r2
        rx=m[a+n] 60 r2
        rx= -65536 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 33024 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 60 r2
        rx=m[a+n] 60 r2
        rx= 65536 r3
        rx=m[a+n] 64 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2576:
        a=rx sp
        rx=m[a+n] 60 r2
        ; -- inline end --
INLINE_RET_i2574:
        a=rx sp
        rx=m[a+n] 68 r2
        ; -- inline end --
        ; 12471: wr_EgressEthernetTypeforVLANtag_typeValue(0x88a8);
        ; -- inline begin: result in __inline_result_i2577 --
        rx= 88 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 56 r2
        ; 720: address = 61614;
        ; 721: int entry;
        rx= 61614 r2
        m[a+n]=rx 52 r2
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        rx= 65536 r2
        rx=m[a+n] 52 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 48 r2
        rx=m[a+n] 48 r2
        rx= 65535 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 56 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2577:
        a=rx sp
        rx=m[a+n] 56 r2
        ; -- inline end --
        ; 12472: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        ; -- inline begin: result in __inline_result_i2579 --
        ; 727: address = 61614;
        ; 728: int entry;
        rx= 61614 r2
        m[a+n]=rx 44 r2
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        rx= 65536 r2
        rx=m[a+n] 44 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 40 r2
        rx=m[a+n] 40 r2
        rx= -65536 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 34984 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 40 r2
        rx=m[a+n] 40 r2
        rx= 65536 r3
        rx=m[a+n] 44 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2581:
        a=rx sp
        rx=m[a+n] 40 r2
        ; -- inline end --
INLINE_RET_i2579:
        a=rx sp
        rx=m[a+n] 48 r2
        ; -- inline end --
        ; 12473: wr_EgressEthernetTypeforVLANtag_typeValue(0x1ffff);
        ; -- inline begin: result in __inline_result_i2582 --
        rx= 88 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 36 r2
        ; 720: address = 61614;
        ; 721: int entry;
        rx= 61614 r2
        m[a+n]=rx 32 r2
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        rx= 65536 r2
        rx=m[a+n] 32 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 28 r2
        rx=m[a+n] 28 r2
        rx= 65535 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 36 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2582:
        a=rx sp
        rx=m[a+n] 36 r2
        ; -- inline end --
        ; 12474: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        ; -- inline begin: result in __inline_result_i2584 --
        ; 727: address = 61614;
        ; 728: int entry;
        rx= 61614 r2
        m[a+n]=rx 24 r2
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        rx= 65536 r2
        rx=m[a+n] 24 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        rx= -65536 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        rx= 65535 r3
        a=rx r3
        a|=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 20 r2
        rx=m[a+n] 20 r2
        rx= 65536 r3
        rx=m[a+n] 24 r4
        a=rx r4
        a+=rx r3
        m[a]=rx r2
INLINE_RET_i2586:
        a=rx sp
        rx=m[a+n] 20 r2
        ; -- inline end --
INLINE_RET_i2584:
        a=rx sp
        rx=m[a+n] 28 r2
        ; -- inline end --
        ; 12475: }
        ; -- inline begin: result in __inline_result_i2587 --
        rx= 88 r2
        a+=rx r2
        rx=a r2
        a=rx sp
        m[a+n]=rx 16 r2
        ; 720: address = 61614;
        ; 721: int entry;
        rx= 61614 r2
        m[a+n]=rx 12 r2
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        rx= 65536 r2
        rx=m[a+n] 12 r3
        a=rx r3
        a+=rx r2
        rx=m[a] r2
        a=rx sp
        m[a+n]=rx 8 r2
        rx=m[a+n] 8 r2
        rx= 65535 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 16 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i2587:
        a=rx sp
        rx=m[a+n] 16 r2
        ; -- inline end --
INLINE_RET_i2568:
        a=rx sp
        rx=m[a+n] 92 r2
        a=rx r2
        ; -- inline end --
        ; 12536: return 0;
        jsr test_hashbasedl3routingtable_destipaddr
        ; 12537: }
        rx= 0 p0
MAIN_END102:
        a=rx sp
        rx= 172 r0
        a+=rx r0
        rx=a sp
        pop-r r4
        pop-a
        j-a
