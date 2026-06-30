;;; Computing Hangul syllable names_START:
        rx= 32768 sp
        jsr main
_HALT:
        j _halt
        ; ======== function readFromDevice ========
        ; frame-size: 0, params: 2, max-reg: 2, leaf: yes, vreg: yes, spills: 0
READFROMDEVICE:
        push-r r2
        ; 3: }
        rx= 65536 r2
        a=rx r2
        rx=a r2
        a=rx p0
        a+=rx r2
        rx=m[a] r2
        a=rx r2
        rx=a p0
READFROMDEVICE_END1:
        pop-r r2
        a=rx srp
        j-a
        ; ======== function writeToDevice ========
        ; frame-size: 0, params: 3, max-reg: 3, leaf: yes, vreg: yes, spills: 0
WRITETODEVICE:
        push-r r3
        ; 6: }
        a=rx p1
        rx=a r2
        rx= 65536 r3
        a=rx r3
        rx=a r3
        a=rx p0
        a+=rx r3
        m[a]=rx r2
WRITETODEVICE_END2:
        pop-r r3
        a=rx srp
        j-a
        ; ======== function rd_CoreTickConfiguration_clkDivider ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_CORETICKCONFIGURATION_CLKDIVIDER:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 262: address = 2;
        ; 263: int entry;
        a= 2
        rx=a r2
        ; 264: entry = readFromDevice(address,0);
        ; 265: *out = (int)((entry) & 0x1ffff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 266: }
        rx= 131071 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_CORETICKCONFIGURATION_CLKDIVIDER_END3:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_CoreTickConfiguration_clkDivider ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_CORETICKCONFIGURATION_CLKDIVIDER:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 269: address = 2;
        ; 270: int entry;
        a= 2
        rx=a r2
        ; 271: entry = readFromDevice(address,0);
        ; 272: entry = (entry & ~(int)0x1ffff) | ((int)(clkDivider & 0x1ffff));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 273: writeToDevice(address,entry,0);
        rx= -131072 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 131071 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 274: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_CORETICKCONFIGURATION_CLKDIVIDER_END4:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_CoreTickConfiguration_stepDivider ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_CORETICKCONFIGURATION_STEPDIVIDER:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 277: address = 2;
        ; 278: int entry;
        a= 2
        rx=a r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 281: }
        a=rx r2
        rx=a r2
        rx= 17 r3
        a=rx r3
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 15 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_CORETICKCONFIGURATION_STEPDIVIDER_END5:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_CoreTickConfiguration_stepDivider ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_CORETICKCONFIGURATION_STEPDIVIDER:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 284: address = 2;
        ; 285: int entry;
        a= 2
        rx=a r2
        ; 286: entry = readFromDevice(address,0);
        ; 287: entry = (entry & ~((int)0xf << 17)) | ((int)(stepDivider & 0xf) << 17);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 288: writeToDevice(address,entry,0);
        rx= -1966081 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 15 r6
        a=rx r6
        a&=rx r5
        rx=a r5
        rx= 17 r6
        a=rx r6
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 289: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_CORETICKCONFIGURATION_STEPDIVIDER_END6:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_CoreTickSelect_clkSelect ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_CORETICKSELECT_CLKSELECT:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 292: address = 3;
        ; 293: int entry;
        a= 3
        rx=a r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 296: }
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_CORETICKSELECT_CLKSELECT_END7:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_CoreTickSelect_clkSelect ========
        ; frame-size: 0, params: 1, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_CORETICKSELECT_CLKSELECT:
        push-srp
        push-r r5
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 299: address = 3;
        ; 300: int entry;
        a= 3
        rx=a r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 303: writeToDevice(address,entry,0);
        a= -4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        a= 3
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 304: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_CORETICKSELECT_CLKSELECT_END8:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_ERMRedConfiguration_redXoff ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_ERMREDCONFIGURATION_REDXOFF:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 600: address = 57850;
        ; 601: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 604: }
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_ERMREDCONFIGURATION_REDXOFF_END9:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_ERMRedConfiguration_redXoff ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_ERMREDCONFIGURATION_REDXOFF:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 607: address = 57850;
        ; 608: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 609: entry = readFromDevice(address,0);
        ; 610: entry = (entry & ~(int)0xfff) | ((int)(redXoff & 0xfff));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 611: writeToDevice(address,entry,0);
        rx= -4096 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 4095 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 612: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_ERMREDCONFIGURATION_REDXOFF_END10:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_ERMRedConfiguration_redXon ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_ERMREDCONFIGURATION_REDXON:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 615: address = 57850;
        ; 616: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 619: }
        a=rx r2
        rx=a r2
        rx= 12 r3
        a=rx r3
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_ERMREDCONFIGURATION_REDXON_END11:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_ERMRedConfiguration_redXon ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_ERMREDCONFIGURATION_REDXON:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 622: address = 57850;
        ; 623: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 624: entry = readFromDevice(address,0);
        ; 625: entry = (entry & ~((int)0xfff << 12)) | ((int)(redXon & 0xfff) << 12);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 626: writeToDevice(address,entry,0);
        rx= -16773121 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 4095 r6
        a=rx r6
        a&=rx r5
        rx=a r5
        rx= 12 r6
        a=rx r6
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 627: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_ERMREDCONFIGURATION_REDXON_END12:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_ERMRedConfiguration_redMaxCells ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_ERMREDCONFIGURATION_REDMAXCELLS:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 630: address = 57850;
        ; 631: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 634: }
        a=rx r2
        rx=a r2
        rx= 24 r3
        a=rx r3
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 255 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_ERMREDCONFIGURATION_REDMAXCELLS_END13:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_ERMRedConfiguration_redMaxCells ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_ERMREDCONFIGURATION_REDMAXCELLS:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 637: address = 57850;
        ; 638: int entry;
        rx= 57850 r2
        a=rx r2
        rx=a r2
        ; 639: entry = readFromDevice(address,0);
        ; 640: entry = (entry & ~((int)0xff << 24)) | ((int)(redMaxCells & 0xff) << 24);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 641: writeToDevice(address,entry,0);
        rx= 16777215 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 255 r6
        a=rx r6
        a&=rx r5
        rx=a r5
        rx= 24 r6
        a=rx r6
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 642: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_ERMREDCONFIGURATION_REDMAXCELLS_END14:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_EgressEthernetTypeforVLANtag_typeValue ========
        ; frame-size: 0, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_EGRESSETHERNETTYPEFORVLANTAG_TYPEVALUE:
        push-srp
        push-r r3
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 720: address = 61614;
        ; 721: int entry;
        rx= 61614 r2
        a=rx r2
        rx=a r2
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 724: }
        rx= 65535 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        m[a]=rx r2
RD_EGRESSETHERNETTYPEFORVLANTAG_TYPEVALUE_END15:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_EgressEthernetTypeforVLANtag_typeValue ========
        ; frame-size: 0, params: 1, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_EGRESSETHERNETTYPEFORVLANTAG_TYPEVALUE:
        push-srp
        push-r r6
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 727: address = 61614;
        ; 728: int entry;
        rx= 61614 r2
        a=rx r2
        rx=a r2
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 731: writeToDevice(address,entry,0);
        rx= -65536 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 0 r5
        a=rx r5
        rx=a r5
        rx= 65535 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 732: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSETHERNETTYPEFORVLANTAG_TYPEVALUE_END16:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function wr_EgressPortConfiguration_colorRemap ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_EGRESSPORTCONFIGURATION_COLORREMAP:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 826: address = 58126;
        ; 827: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 828: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 829: entry = readFromDevice(address,0);
        ; 830: entry = (entry & ~(int)0x1) | ((int)(colorRemap & 0x1));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 831: writeToDevice(address,entry,0);
        a= -2
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 832: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSPORTCONFIGURATION_COLORREMAP_END17:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_EgressPortConfiguration_vid ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_EGRESSPORTCONFIGURATION_VID:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 920: address = 58126;
        ; 921: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 922: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 923: entry = readFromDevice(address,0);
        ; 924: *out = (int)((entry >> 12) & 0xfff);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 925: }
        a=rx r2
        rx=a r2
        rx= 12 r3
        a=rx r3
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_EGRESSPORTCONFIGURATION_VID_END18:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_EgressPortConfiguration_vid ========
        ; frame-size: 0, params: 2, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_EGRESSPORTCONFIGURATION_VID:
        push-srp
        push-r r6
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 928: address = 58126;
        ; 929: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 930: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 931: entry = readFromDevice(address,0);
        ; 932: entry = (entry & ~((int)0xfff << 12)) | ((int)(vid & 0xfff) << 12);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 933: writeToDevice(address,entry,0);
        rx= -16773121 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        rx= 4095 r6
        a=rx r6
        a&=rx r5
        rx=a r5
        rx= 12 r6
        a=rx r6
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 934: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSPORTCONFIGURATION_VID_END19:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_EgressPortConfiguration_moreThanOneVlans ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_EGRESSPORTCONFIGURATION_MORETHANONEVLANS:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1022: address = 58126;
        ; 1023: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 1024: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1025: entry = readFromDevice(address,0);
        ; 1026: *out = (int)((entry >> 31) & 0x1);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1027: }
        a=rx r2
        rx=a r2
        rx= 31 r3
        a=rx r3
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_EGRESSPORTCONFIGURATION_MORETHANONEVLANS_END20:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_EgressPortConfiguration_moreThanOneVlans ========
        ; frame-size: 0, params: 2, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_EGRESSPORTCONFIGURATION_MORETHANONEVLANS:
        push-srp
        push-r r6
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1030: address = 58126;
        ; 1031: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 1032: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1033: entry = readFromDevice(address,0);
        ; 1034: entry = (entry & ~((int)0x1 << 31)) | ((int)(moreThanOneVlans & 0x1) << 31);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1035: writeToDevice(address,entry,0);
        rx= 2147483647 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        rx=a r5
        rx= 31 r6
        a=rx r6
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 1036: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSPORTCONFIGURATION_MORETHANONEVLANS_END21:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        ; ======== function rd_EgressPortConfiguration_dropUntaggedVlans ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1039: address = 58126;
        ; 1040: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 1041: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1042: entry = readFromDevice(address+1,0);
        ; 1043: *out = (int)((entry) & 0x1);
        a= 1
        a+=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1044: }
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS_END22:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_EgressPortConfiguration_dropUntaggedVlans ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1047: address = 58126;
        ; 1048: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 1049: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1050: entry = readFromDevice(address+1,0);
        ; 1051: entry = (entry & ~(int)0x1) | ((int)(dropUntaggedVlans & 0x1));
        a= 1
        a+=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1052: writeToDevice(address+1,entry,0);
        a= -2
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 1053: }
        a= 1
        a+=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS_END23:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function wr_EgressPortConfiguration_dropSingleTaggedVlans ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_EGRESSPORTCONFIGURATION_DROPSINGLETAGGEDVLANS:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1064: address = 58126;
        ; 1065: address += idx*2;
        rx= 58126 r2
        a=rx r2
        rx=a r2
        ; 1066: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1067: entry = readFromDevice(address+1,0);
        ; 1068: entry = (entry & ~((int)0x1 << 1)) | ((int)(dropSingleTaggedVlans & 0x1) << 1);
        a= 1
        a+=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1069: writeToDevice(address+1,entry,0);
        a= -3
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        rx=a r5
        a=rx r5
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1070: }
        a= 1
        a+=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_EGRESSPORTCONFIGURATION_DROPSINGLETAGGEDVLANS_END24:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HairpinEnable_allowFlood ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HAIRPINENABLE_ALLOWFLOOD:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1733: address = 56021;
        ; 1734: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1735: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1736: entry = readFromDevice(address,0);
        ; 1737: *out = (int)((entry) & 0x1);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1738: }
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HAIRPINENABLE_ALLOWFLOOD_END25:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HairpinEnable_allowFlood ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_HAIRPINENABLE_ALLOWFLOOD:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1743: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1744: entry = readFromDevice(address,0);
        ; 1745: entry = (entry & ~(int)0x1) | ((int)(allowFlood & 0x1));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1746: writeToDevice(address,entry,0);
        a= -2
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 1747: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HAIRPINENABLE_ALLOWFLOOD_END26:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HairpinEnable_allowMc ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HAIRPINENABLE_ALLOWMC:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1750: address = 56021;
        ; 1751: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1752: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1753: entry = readFromDevice(address,0);
        ; 1754: *out = (int)((entry >> 1) & 0x1);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1755: }
        a=rx r2
        rx=a r2
        a=rx r2
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HAIRPINENABLE_ALLOWMC_END27:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HairpinEnable_allowMc ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_HAIRPINENABLE_ALLOWMC:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1758: address = 56021;
        ; 1759: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1760: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1761: entry = readFromDevice(address,0);
        ; 1762: entry = (entry & ~((int)0x1 << 1)) | ((int)(allowMc & 0x1) << 1);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1763: writeToDevice(address,entry,0);
        a= -3
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        rx=a r5
        a=rx r5
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1764: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HAIRPINENABLE_ALLOWMC_END28:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HairpinEnable_allowUc ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HAIRPINENABLE_ALLOWUC:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1767: address = 56021;
        ; 1768: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1769: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1770: entry = readFromDevice(address,0);
        ; 1771: *out = (int)((entry >> 2) & 0x1);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1772: }
        a=rx r2
        rx=a r2
        a=rx r2
        a=a>>1
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HAIRPINENABLE_ALLOWUC_END29:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HairpinEnable_allowUc ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_HAIRPINENABLE_ALLOWUC:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1775: address = 56021;
        ; 1776: address += idx*1;
        rx= 56021 r2
        a=rx r2
        rx=a r2
        ; 1777: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a+=rx r2
        rx=a r2
        ; 1778: entry = readFromDevice(address,0);
        ; 1779: entry = (entry & ~((int)0x1 << 2)) | ((int)(allowUc & 0x1) << 2);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1780: writeToDevice(address,entry,0);
        a= -5
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 1
        a&=rx r5
        rx=a r5
        a=rx r5
        a=a<<1
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1781: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HAIRPINENABLE_ALLOWUC_END30:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HashBasedL3RoutingTable_proto ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HASHBASEDL3ROUTINGTABLE_PROTO:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1869: address = 12887;
        ; 1870: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1871: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1872: entry = readFromDevice(address,0);
        ; 1873: *out = (int)((entry) & 0x3);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1874: }
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HASHBASEDL3ROUTINGTABLE_PROTO_END31:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HashBasedL3RoutingTable_proto ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_HASHBASEDL3ROUTINGTABLE_PROTO:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1877: address = 12887;
        ; 1878: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1879: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1880: entry = readFromDevice(address,0);
        ; 1881: entry = (entry & ~(int)0x3) | ((int)(proto & 0x3));
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1882: writeToDevice(address,entry,0);
        a= -4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 3
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 1883: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HASHBASEDL3ROUTINGTABLE_PROTO_END32:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HashBasedL3RoutingTable_vrf ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HASHBASEDL3ROUTINGTABLE_VRF:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1886: address = 12887;
        ; 1887: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1888: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1889: entry = readFromDevice(address,0);
        ; 1890: *out = (int)((entry >> 2) & 0x3);
        a=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1891: }
        a=rx r2
        rx=a r2
        a=rx r2
        a=a>>1
        a=a>>1
        rx=a r2
        a= 3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HASHBASEDL3ROUTINGTABLE_VRF_END33:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HashBasedL3RoutingTable_vrf ========
        ; frame-size: 0, params: 2, max-reg: 5, leaf: no, vreg: yes, spills: 0
WR_HASHBASEDL3ROUTINGTABLE_VRF:
        push-srp
        push-r r5
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1894: address = 12887;
        ; 1895: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1896: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1897: entry = readFromDevice(address,0);
        ; 1898: entry = (entry & ~((int)0x3 << 2)) | ((int)(vrf & 0x3) << 2);
        a=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1899: writeToDevice(address,entry,0);
        rx= -13 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        a= 3
        a&=rx r5
        rx=a r5
        a=rx r5
        a=a<<1
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1900: }
        a=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HASHBASEDL3ROUTINGTABLE_VRF_END34:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function rd_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 8, params: 2, max-reg: 9, leaf: no, vreg: yes, spills: 2
RD_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -16 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 8 p0
        a=rx sp
        m[a+n]=rx 12 p1
        ; 1903: address = 12887;
        ; 1904: address += idx*8;
        rx= 12887 r0
        a=rx r0
        rx=a r6
        ; 1905: int i, j;
        a=rx sp
        rx=m[a+n] 8 r0
        a=rx r0
        rx=a r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        ; 1906: int prev = readFromDevice(address,0);
        ; 1907: for (i = 0; i < 4; i++) {
        a=rx r6
        rx=a r0
        a= 0
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r9
        ; 1914: }
        a= 0
        rx=a r7
FOR42:
        a= 4
        rx=a r0
        a=rx r7
        a-=rx r0
        jge endfor44
        ; 1909: int merged = (prev >> 4) | (curr << 28);
        a= 1
        a+=rx r6
        rx=a r1
        a=rx r7
        a+=rx r1
        rx=a r0
        a= 0
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
        ; 1910: prev = curr;
        a=rx r9
        rx=a r0
        a= 4
        rx=a p1
        a=rx r0
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r0
        a=rx sp
        rx=m[a+n] 4 r1
        a=rx r1
        rx=a r1
        rx= 28 r2
        a=rx r2
        rx=a p1
        a=rx r1
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        ; 1911: for (j = 0; j < 4; j++)
        a=rx sp
        rx=m[a+n] 4 r0
        a=rx r0
        rx=a r9
        ; 1913: }
        a= 0
        rx=a r8
FOR45:
        a= 4
        rx=a r0
        a=rx r8
        a-=rx r0
        jge endfor47
        ; 1913: }
        a=rx sp
        rx=m[a+n] 0 r0
        a=rx r0
        rx=a r0
        a=rx r8
        rx=a r1
        a=rx r1
        a=a<<1
        a=a<<1
        a=a<<1
        rx=a p1
        a=rx r0
        rx=a p0
        jsr __shr
        a=rx p0
        mask-a-b
        rx=a r0
        a=rx sp
        rx=m[a+n] 12 r1
        a=rx r1
        rx=a r1
        a=rx r7
        rx=a r2
        a=rx r2
        a=a<<1
        a=a<<1
        rx=a r2
        a=rx r8
        a+=rx r2
        a+=rx r1
        m[a].b=rx r0
FORCONT46:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for45
ENDFOR47:
FORCONT43:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for42
ENDFOR44:
RD_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END35:
        a=rx sp
        rx= 16 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        ; ======== function wr_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 12, params: 2, max-reg: 9, leaf: no, vreg: yes, spills: 3
WR_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -20 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 12 p0
        a=rx sp
        m[a+n]=rx 16 p1
        ; 1917: address = 12887;
        ; 1918: address += idx*8;
        rx= 12887 r0
        a=rx r0
        rx=a r6
        ; 1919: int i, j;
        a=rx sp
        rx=m[a+n] 12 r0
        a=rx r0
        rx=a r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        ; 1920: int v;
        ; 1921: int wprev = readFromDevice(address,0) & 0xf;
        ; 1922: int wlast = readFromDevice(address+4,0) & 0xfffffff0;
        a=rx r6
        rx=a r0
        a= 0
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        rx= 15 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 8 r0
        ; 1923: for (i = 0; i < 4; i++) {
        a= 4
        a+=rx r6
        rx=a r0
        a= 0
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        rx= 4294967280 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
        ; 1933: writeToDevice(address+4, wprev, 0);
        a= 0
        rx=a r7
FOR61:
        a= 4
        rx=a r0
        a=rx r7
        a-=rx r0
        jge endfor63
        ; 1925: for (j = 0; j < 4; j++) {
        a= 3
        rx=a r0
        a=rx r7
        a-=rx r0
        jz cmptrue66
        a= 0
        j cmpend67
CMPTRUE66:
        a= 1
CMPEND67:
        rx= 0 r0
        a-=rx r0
        jz ternelse64
        a=rx sp
        rx=m[a+n] 4 r0
        a=rx r0
        j ternend65
TERNELSE64:
        a= 0
TERNEND65:
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        ; 1930: writeToDevice(address+i, wprev, 0);
        a= 0
        rx=a r8
FOR68:
        a= 4
        rx=a r0
        a=rx r8
        a-=rx r0
        jge endfor70
        ; 1927: wprev |= v << (4 + j*8);
        a=rx sp
        rx=m[a+n] 16 r0
        a=rx r0
        rx=a r0
        a=rx r7
        rx=a r1
        a=rx r1
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
        rx=m[a+n] 8 r0
        a=rx r0
        rx=a r0
        a=rx r9
        rx=a r1
        a= 4
        rx=a r2
        a=rx r8
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a p1
        a=rx r1
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 8 r0
        ; 1929: }
        rx= 28 r0
        a=rx r0
        rx=a r0
        a=rx r8
        rx=a r1
        a=rx r1
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
        jge endif72
        ; 1929: }
        a=rx sp
        rx=m[a+n] 0 r0
        a=rx r0
        rx=a r0
        a=rx r9
        rx=a r1
        rx= 28 r2
        a=rx r2
        rx=a r2
        a=rx r8
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        rx=a r3
        a=rx r2
        a-=rx r3
        rx=a p1
        a=rx r1
        rx=a p0
        jsr __shr
        a=rx p0
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
ENDIF72:
FORCONT69:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for68
ENDFOR70:
        ; 1931: wprev = wcurr;
        a=rx r7
        a+=rx r6
        rx=a r0
        a=rx sp
        rx=m[a+n] 8 r2
        a=rx r2
        rx=a r1
        a= 0
        rx=a r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
        a=rx p0
        ; 1932: }
        a=rx sp
        rx=m[a+n] 0 r0
        a=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 8 r0
FORCONT62:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for61
ENDFOR63:
        ; 1934: }
        a= 4
        a+=rx r6
        rx=a r0
        a=rx sp
        rx=m[a+n] 8 r2
        a=rx r2
        rx=a r1
        a= 0
        rx=a r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END48:
        a=rx sp
        rx= 20 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        ; ======== function rd_HashBasedL3RoutingTable_nextHopPointer ========
        ; frame-size: 0, params: 2, max-reg: 3, leaf: no, vreg: yes, spills: 0
RD_HASHBASEDL3ROUTINGTABLE_NEXTHOPPOINTER:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1937: address = 12887;
        ; 1938: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1939: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1940: entry = readFromDevice(address+4,0);
        ; 1941: *out = (int)((entry >> 4) & 0x1ff);
        a= 4
        a+=rx r2
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        ; 1942: }
        a=rx r2
        rx=a r2
        a= 4
        rx=a p1
        a=rx r2
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 511 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HASHBASEDL3ROUTINGTABLE_NEXTHOPPOINTER_END73:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function wr_HashBasedL3RoutingTable_nextHopPointer ========
        ; frame-size: 0, params: 2, max-reg: 6, leaf: no, vreg: yes, spills: 0
WR_HASHBASEDL3ROUTINGTABLE_NEXTHOPPOINTER:
        push-srp
        push-r r6
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        a=rx sp
        m[a+n]=rx 4 p1
        ; 1945: address = 12887;
        ; 1946: address += idx*8;
        rx= 12887 r2
        a=rx r2
        rx=a r2
        ; 1947: int entry;
        a=rx sp
        rx=m[a+n] 0 r3
        a=rx r3
        rx=a r3
        a=rx r3
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1948: entry = readFromDevice(address+4,0);
        ; 1949: entry = (entry & ~((int)0x1ff << 4)) | ((int)(nextHopPointer & 0x1ff) << 4);
        a= 4
        a+=rx r2
        rx=a r3
        a= 0
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        ; 1950: writeToDevice(address+4,entry,0);
        rx= -8177 r4
        a=rx r4
        a&=rx r3
        rx=a r4
        a=rx sp
        rx=m[a+n] 4 r5
        a=rx r5
        rx=a r5
        rx= 511 r6
        a=rx r6
        a&=rx r5
        rx=a r5
        a= 4
        rx=a p1
        a=rx r5
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r4
        rx=a r3
        ; 1951: }
        a= 4
        a+=rx r2
        rx=a r2
        a=rx r3
        rx=a r3
        a= 0
        rx=a r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
        a=rx p0
WR_HASHBASEDL3ROUTINGTABLE_NEXTHOPPOINTER_END74:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r6
        pop-a
        j-a
        lalign-dword 0
G_PASS:
        adword 0
        lalign-dword 0
G_FAIL:
        adword 0
        ; ======== function memset ========
        ; frame-size: 0, params: 3, max-reg: 8, leaf: yes, vreg: yes, spills: 0
MEMSET:
        push-r r8
        ; 12368: while (n--) *p++ = (unsigned char)c;
        a=rx p0
        rx=a r2
        ; 12369: return s;
WHILE76:
        a=rx p2
        rx=a r3
        rx= -1 r4
        a+=rx r4
        rx=a p2
        a=rx r3
        rx= 0 r5
        a-=rx r5
        jz endwhile77
        ; 12369: return s;
        a=rx p1
        mask-a-b
        rx=a r6
        a=rx r2
        rx=a r7
        rx= 1 r8
        a+=rx r8
        rx=a r2
        a=rx r7
        m[a].b=rx r6
        j while76
ENDWHILE77:
        ; 12370: }
        a=rx p0
        rx=a p0
MEMSET_END75:
        pop-r r8
        a=rx srp
        j-a
        ; ======== function test_CoreTickConfiguration ========
        ; frame-size: 4, params: 1, max-reg: 2, leaf: no, vreg: yes, spills: 1
TEST_CORETICKCONFIGURATION:
        push-srp
        push-r r2
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12382: wr_CoreTickConfiguration_clkDivider(0x64);
        ; 12383: wr_CoreTickConfiguration_stepDivider(0xa);
        rx= 100 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_clkdivider
        a=rx p0
        ; 12384: rd_CoreTickConfiguration_clkDivider(&val);
        rx= 10 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        a=rx p0
        ; 12385: rd_CoreTickConfiguration_stepDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        a=rx p0
        ; 12386: wr_CoreTickConfiguration_clkDivider(0x1234 & 0x1ffff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        a=rx p0
        ; 12387: rd_CoreTickConfiguration_clkDivider(&val);
        rx= 4660 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_clkdivider
        a=rx p0
        ; 12388: rd_CoreTickConfiguration_stepDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        a=rx p0
        ; 12389: wr_CoreTickConfiguration_stepDivider(0xf);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        a=rx p0
        ; 12390: rd_CoreTickConfiguration_stepDivider(&val);
        rx= 15 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        a=rx p0
        ; 12391: rd_CoreTickConfiguration_clkDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        a=rx p0
        ; 12392: wr_CoreTickConfiguration_stepDivider(0xff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        a=rx p0
        ; 12393: rd_CoreTickConfiguration_stepDivider(&val);
        rx= 255 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        a=rx p0
        ; 12394: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        a=rx p0
TEST_CORETICKCONFIGURATION_END80:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r2
        pop-a
        j-a
        ; ======== function test_CoreTickSelect ========
        ; frame-size: 4, params: 1, max-reg: 2, leaf: no, vreg: yes, spills: 1
TEST_CORETICKSELECT:
        push-srp
        push-r r2
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12397: wr_CoreTickSelect_clkSelect(1);
        ; 12398: rd_CoreTickSelect_clkSelect(&val);
        a= 1
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        a=rx p0
        ; 12399: wr_CoreTickSelect_clkSelect(2);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        a=rx p0
        ; 12400: rd_CoreTickSelect_clkSelect(&val);
        a= 2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        a=rx p0
        ; 12401: wr_CoreTickSelect_clkSelect(3);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        a=rx p0
        ; 12402: rd_CoreTickSelect_clkSelect(&val);
        a= 3
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        a=rx p0
        ; 12403: wr_CoreTickSelect_clkSelect(0xff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        a=rx p0
        ; 12404: rd_CoreTickSelect_clkSelect(&val);
        rx= 255 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        a=rx p0
        ; 12405: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        a=rx p0
TEST_CORETICKSELECT_END81:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r2
        pop-a
        j-a
        ; ======== function test_ERMRedConfiguration ========
        ; frame-size: 4, params: 1, max-reg: 2, leaf: no, vreg: yes, spills: 1
TEST_ERMREDCONFIGURATION:
        push-srp
        push-r r2
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12408: wr_ERMRedConfiguration_redXoff(0xcc);
        ; 12409: wr_ERMRedConfiguration_redXon(0x200);
        rx= 204 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxoff
        a=rx p0
        ; 12410: wr_ERMRedConfiguration_redMaxCells(0xf);
        rx= 512 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxon
        a=rx p0
        ; 12411: rd_ERMRedConfiguration_redXoff(&val);
        rx= 15 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12412: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        a=rx p0
        ; 12413: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        a=rx p0
        ; 12414: wr_ERMRedConfiguration_redXoff(0xabc);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12415: rd_ERMRedConfiguration_redXoff(&val);
        rx= 2748 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxoff
        a=rx p0
        ; 12416: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        a=rx p0
        ; 12417: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        a=rx p0
        ; 12418: wr_ERMRedConfiguration_redXon(0x7ff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12419: rd_ERMRedConfiguration_redXon(&val);
        rx= 2047 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxon
        a=rx p0
        ; 12420: rd_ERMRedConfiguration_redXoff(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        a=rx p0
        ; 12421: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        a=rx p0
        ; 12422: wr_ERMRedConfiguration_redMaxCells(0xaa);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12423: rd_ERMRedConfiguration_redMaxCells(&val);
        rx= 170 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12424: rd_ERMRedConfiguration_redXoff(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        a=rx p0
        ; 12425: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        a=rx p0
        ; 12426: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        a=rx p0
TEST_ERMREDCONFIGURATION_END82:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r2
        pop-a
        j-a
        ; ======== function test_HairpinEnable ========
        ; frame-size: 4, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 1
TEST_HAIRPINENABLE:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12429: wr_HairpinEnable_allowFlood(0, 0);
        ; 12430: wr_HairpinEnable_allowMc(0, 0);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        a=rx p0
        ; 12431: wr_HairpinEnable_allowUc(0, 1);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowmc
        a=rx p0
        ; 12432: rd_HairpinEnable_allowFlood(0, &val);
        a= 0
        rx=a r2
        a= 1
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowuc
        a=rx p0
        ; 12433: rd_HairpinEnable_allowMc(0, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        a=rx p0
        ; 12434: rd_HairpinEnable_allowUc(0, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowmc
        a=rx p0
        ; 12435: wr_HairpinEnable_allowFlood(0, 1);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
        a=rx p0
        ; 12436: rd_HairpinEnable_allowFlood(0, &val);
        a= 0
        rx=a r2
        a= 1
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        a=rx p0
        ; 12437: rd_HairpinEnable_allowMc(0, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        a=rx p0
        ; 12438: rd_HairpinEnable_allowUc(0, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowmc
        a=rx p0
        ; 12439: wr_HairpinEnable_allowFlood(1, 0);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
        a=rx p0
        ; 12440: wr_HairpinEnable_allowMc(1, 0);
        a= 1
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        a=rx p0
        ; 12441: wr_HairpinEnable_allowUc(1, 1);
        a= 1
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowmc
        a=rx p0
        ; 12442: wr_HairpinEnable_allowFlood(0, 0);
        a= 1
        rx=a r2
        a= 1
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowuc
        a=rx p0
        ; 12443: rd_HairpinEnable_allowFlood(1, &val);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        a=rx p0
        ; 12444: rd_HairpinEnable_allowUc(1, &val);
        a= 1
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        a=rx p0
        ; 12445: }
        a= 1
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
        a=rx p0
TEST_HAIRPINENABLE_END83:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function test_EgressPortConfiguration_crossword ========
        ; frame-size: 4, params: 1, max-reg: 3, leaf: no, vreg: yes, spills: 1
TEST_EGRESSPORTCONFIGURATION_CROSSWORD:
        push-srp
        push-r r3
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12448: int idx = 0;
        ; 12450: wr_EgressPortConfiguration_vid(idx, 0);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_colorremap
        a=rx p0
        ; 12451: wr_EgressPortConfiguration_moreThanOneVlans(idx, 0);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_vid
        a=rx p0
        ; 12452: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 0);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_morethanonevlans
        a=rx p0
        ; 12453: wr_EgressPortConfiguration_dropSingleTaggedVlans(idx, 0);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_dropuntaggedvlans
        a=rx p0
        ; 12454: wr_EgressPortConfiguration_moreThanOneVlans(idx, 1);
        a= 0
        rx=a r2
        a= 0
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_dropsingletaggedvlans
        a=rx p0
        ; 12455: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        a= 0
        rx=a r2
        a= 1
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_morethanonevlans
        a=rx p0
        ; 12456: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        a=rx p0
        ; 12457: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 1);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
        a=rx p0
        ; 12458: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        a= 0
        rx=a r2
        a= 1
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_dropuntaggedvlans
        a=rx p0
        ; 12459: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
        a=rx p0
        ; 12460: wr_EgressPortConfiguration_vid(idx, 0xabc);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        a=rx p0
        ; 12461: rd_EgressPortConfiguration_vid(idx, &val);
        a= 0
        rx=a r2
        rx= 2748 r3
        a=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_egressportconfiguration_vid
        a=rx p0
        ; 12462: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_vid
        a=rx p0
        ; 12463: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        a=rx p0
        ; 12464: }
        a= 0
        rx=a r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
        a=rx p0
TEST_EGRESSPORTCONFIGURATION_CROSSWORD_END84:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function test_EgressEthernetTypeforVLANtag ========
        ; frame-size: 4, params: 1, max-reg: 2, leaf: no, vreg: yes, spills: 1
TEST_EGRESSETHERNETTYPEFORVLANTAG:
        push-srp
        push-r r2
        a=rx sp
        rx= -8 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 4 p0
        ; 12467: wr_EgressEthernetTypeforVLANtag_typeValue(0xffff);
        ; 12468: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 65535 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12469: wr_EgressEthernetTypeforVLANtag_typeValue(0x8100);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12470: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 33024 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12471: wr_EgressEthernetTypeforVLANtag_typeValue(0x88a8);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12472: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 34984 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12473: wr_EgressEthernetTypeforVLANtag_typeValue(0x1ffff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12474: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 131071 r2
        a=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        a=rx p0
        ; 12475: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        a=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        a=rx p0
TEST_EGRESSETHERNETTYPEFORVLANTAG_END85:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r2
        pop-a
        j-a
        ; ======== function test_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 96, params: 1, max-reg: 7, leaf: no, vreg: yes, spills: 24
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r7
        a=rx sp
        rx= -100 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 96 p0
        ; 12495: unsigned char in[16];
        ; 12496: unsigned char zeros[16] = {0};
        ; 12498: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a=rx sp
        rx= 64 r1
        a+=rx r1
        rx=a r0
        a= 0
        rx=a r1
        rx= 16 r3
        a=rx r3
        rx=a r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        a=rx p0
        ; 12499: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12500: for (int i = 0; i < 16; i++) in[i] = (unsigned char)(i + 1);
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 0
        rx=a r9
FOR98:
        rx= 16 r0
        a=rx r0
        rx=a r0
        a=rx r9
        a-=rx r0
        jge endfor100
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 1
        a+=rx r9
        mask-a-b
        rx=a r0
        a=rx sp
        rx= 64 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT99:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for98
ENDFOR100:
        ; 12502: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12503: memset(in, 0xff, 16);
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12504: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a=rx sp
        rx= 64 r1
        a+=rx r1
        rx=a r0
        rx= 255 r2
        a=rx r2
        rx=a r1
        rx= 16 r3
        a=rx r3
        rx=a r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        a=rx p0
        ; 12505: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12506: for (int i = 0; i < 16; i++) in[i] = (i & 1) ? 0x55 : 0xaa;
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12507: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 0
        rx=a r9
FOR101:
        rx= 16 r0
        a=rx r0
        rx=a r0
        a=rx r9
        a-=rx r0
        jge endfor103
        ; 12507: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 1
        a&=rx r9
        rx= 0 r0
        a-=rx r0
        jz ternelse104
        rx= 85 r0
        a=rx r0
        j ternend105
TERNELSE104:
        rx= 170 r0
        a=rx r0
TERNEND105:
        rx=a r0
        a=rx sp
        rx= 64 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT102:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for101
ENDFOR103:
        ; 12508: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12509: wr_HashBasedL3RoutingTable_proto(idx, 2);
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12510: wr_HashBasedL3RoutingTable_vrf(idx, 1);
        a= 0
        rx=a r0
        a= 2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_proto
        a=rx p0
        ; 12511: for (int i = 0; i < 16; i++) in[i] = (unsigned char)(0x10 + i);
        a= 0
        rx=a r0
        a= 1
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_vrf
        a=rx p0
        ; 12512: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 0
        rx=a r9
FOR106:
        rx= 16 r0
        a=rx r0
        rx=a r0
        a=rx r9
        a-=rx r0
        jge endfor108
        ; 12512: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 16 r0
        a=rx r0
        rx=a r0
        a=rx r9
        a+=rx r0
        mask-a-b
        rx=a r0
        a=rx sp
        rx= 64 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT107:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for106
ENDFOR108:
        ; 12513: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12514: int proto_val, vrf_val;
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12515: rd_HashBasedL3RoutingTable_proto(idx, &proto_val);
        ; 12516: rd_HashBasedL3RoutingTable_vrf(idx, &vrf_val);
        a= 0
        rx=a r0
        a=rx sp
        rx= 44 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_proto
        a=rx p0
        ; 12517: wr_HashBasedL3RoutingTable_nextHopPointer(idx, 0x1aa);
        a= 0
        rx=a r0
        a=rx sp
        rx= 40 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_vrf
        a=rx p0
        ; 12518: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 0
        rx=a r0
        rx= 426 r2
        a=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_nexthoppointer
        a=rx p0
        ; 12519: int nhp;
        a= 0
        rx=a r0
        a=rx sp
        rx= 64 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12520: rd_HashBasedL3RoutingTable_nextHopPointer(idx, &nhp);
        ; 12521: int idx1 = 1;
        a= 0
        rx=a r0
        a=rx sp
        rx= 36 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_nexthoppointer
        a=rx p0
        ; 12523: memset(in1, 0xcc, 16);
        ; 12524: wr_HashBasedL3RoutingTable_destIPAddr(idx1, in1);
        a=rx sp
        rx= 16 r1
        a+=rx r1
        rx=a r0
        rx= 204 r2
        a=rx r2
        rx=a r1
        rx= 16 r3
        a=rx r3
        rx=a r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        a=rx p0
        ; 12525: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        a= 1
        rx=a r0
        a=rx sp
        rx= 16 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12526: rd_HashBasedL3RoutingTable_destIPAddr(idx1, out1);
        a= 0
        rx=a r0
        a=rx sp
        rx= 80 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12527: }
        a= 1
        rx=a r0
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        a=rx p0
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END86:
        a=rx sp
        rx= 100 r0
        a+=rx r0
        rx=a sp
        pop-r r7
        pop-a
        j-a
        ; ======== function main ========
        ; frame-size: 0, params: 1, max-reg: -1, leaf: no, vreg: yes, spills: 0
MAIN:
        push-srp
        a=rx sp
        rx= -4 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 0 p0
        ; 12530: test_CoreTickSelect();
        jsr test_coretickconfiguration
        a=rx p0
        ; 12531: test_ERMRedConfiguration();
        jsr test_coretickselect
        a=rx p0
        ; 12532: test_HairpinEnable();
        jsr test_ermredconfiguration
        a=rx p0
        ; 12533: test_EgressPortConfiguration_crossword();
        jsr test_hairpinenable
        a=rx p0
        ; 12534: test_EgressEthernetTypeforVLANtag();
        jsr test_egressportconfiguration_crossword
        a=rx p0
        ; 12535: test_HashBasedL3RoutingTable_destIPAddr();
        jsr test_egressethernettypeforvlantag
        a=rx p0
        ; 12536: return 0;
        jsr test_hashbasedl3routingtable_destipaddr
        a=rx p0
        ; 12537: }
        a= 0
        rx=a p0
MAIN_END109:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-a
        j-a
        ; ======== runtime: __shl ========
        ; P0 = P0 << P1 (shift one bit per iteration)
__SHL:
        push-r r2
        a=rx p1
        rx=a r0
        a=rx p0
__SHL_LOOP:
        rx=a r1
        a=rx r0
        rx= 0 r2
        a-=rx r2
        jz __shl_end
        a=rx r0
        rx= -1 r2
        a+=rx r2
        rx=a r0
        a=rx r1
        a=a<<1
        j __shl_loop
__SHL_END:
        a=rx r1
        rx=a p0
        pop-r r2
        a=rx srp
        j-a
        ; ======== runtime: __shr ========
        ; P0 = P0 >> P1 (logical, shift one bit per iteration)
__SHR:
        push-r r2
        a=rx p1
        rx=a r0
        a=rx p0
__SHR_LOOP:
        rx=a r1
        a=rx r0
        rx= 0 r2
        a-=rx r2
        jz __shr_end
        a=rx r0
        rx= -1 r2
        a+=rx r2
        rx=a r0
        a=rx r1
        a=a>>1
        j __shr_loop
__SHR_END:
        a=rx r1
        rx=a p0
        pop-r r2
        a=rx srp
        j-a
