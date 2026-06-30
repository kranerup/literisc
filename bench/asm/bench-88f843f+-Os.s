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
        rx= 2 r2
        ; 264: entry = readFromDevice(address,0);
        ; 265: *out = (int)((entry) & 0x1ffff);
        a=rx r2
        rx= 0 r3
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
        rx= 2 r2
        ; 271: entry = readFromDevice(address,0);
        ; 272: entry = (entry & ~(int)0x1ffff) | ((int)(clkDivider & 0x1ffff));
        a=rx r2
        rx=a r3
        rx= 0 r4
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
        rx= 131071 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 274: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        rx= 2 r2
        ; 279: entry = readFromDevice(address,0);
        ; 280: *out = (int)((entry >> 17) & 0xf);
        a=rx r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        rx= 17 p1
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
        rx= 2 r2
        ; 286: entry = readFromDevice(address,0);
        ; 287: entry = (entry & ~((int)0xf << 17)) | ((int)(stepDivider & 0xf) << 17);
        a=rx r2
        rx=a r3
        rx= 0 r4
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
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        rx= 3 r2
        ; 294: entry = readFromDevice(address,0);
        ; 295: *out = (int)((entry) & 0x3);
        a=rx r2
        rx= 0 r3
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
        rx= 3 r2
        ; 301: entry = readFromDevice(address,0);
        ; 302: entry = (entry & ~(int)0x3) | ((int)(clkSelect & 0x3));
        a=rx r2
        rx=a r3
        rx= 0 r4
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
        a= 3
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 304: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        ; 602: entry = readFromDevice(address,0);
        ; 603: *out = (int)((entry) & 0xfff);
        a=rx r2
        rx= 0 r3
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
        ; 609: entry = readFromDevice(address,0);
        ; 610: entry = (entry & ~(int)0xfff) | ((int)(redXoff & 0xfff));
        rx=a r3
        rx= 0 r4
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
        rx= 4095 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 612: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        ; 617: entry = readFromDevice(address,0);
        ; 618: *out = (int)((entry >> 12) & 0xfff);
        a=rx r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        rx= 12 p1
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
        ; 624: entry = readFromDevice(address,0);
        ; 625: entry = (entry & ~((int)0xfff << 12)) | ((int)(redXon & 0xfff) << 12);
        rx=a r3
        rx= 0 r4
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
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        ; 632: entry = readFromDevice(address,0);
        ; 633: *out = (int)((entry >> 24) & 0xff);
        a=rx r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        rx= 24 p1
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
        ; 639: entry = readFromDevice(address,0);
        ; 640: entry = (entry & ~((int)0xff << 24)) | ((int)(redMaxCells & 0xff) << 24);
        rx=a r3
        rx= 0 r4
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
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
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
        ; 722: entry = readFromDevice(address,0);
        ; 723: *out = (int)((entry) & 0xffff);
        a=rx r2
        rx= 0 r3
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
        ; 729: entry = readFromDevice(address,0);
        ; 730: entry = (entry & ~(int)0xffff) | ((int)(typeValue & 0xffff));
        rx=a r3
        rx= 0 r4
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
        rx= 65535 r6
        a=rx r6
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 732: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_EGRESSETHERNETTYPEFORVLANTAG_TYPEVALUE_END16:
        a=rx sp
        rx= 4 r0
        a+=rx r0
        rx=a sp
        pop-r r6
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
        m[a+n]=rx 4 p1
        ; 928: address = 58126;
        ; 929: address += idx*2;
        rx= 58126 r2
        ; 930: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        rx=a r3
        rx= 0 r4
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
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_EGRESSPORTCONFIGURATION_VID_END17:
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
        m[a+n]=rx 4 p1
        ; 1022: address = 58126;
        ; 1023: address += idx*2;
        rx= 58126 r2
        ; 1024: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        rx= 31 p1
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
RD_EGRESSPORTCONFIGURATION_MORETHANONEVLANS_END18:
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
        m[a+n]=rx 4 p1
        ; 1030: address = 58126;
        ; 1031: address += idx*2;
        rx= 58126 r2
        ; 1032: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        rx=a r3
        rx= 0 r4
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
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_EGRESSPORTCONFIGURATION_MORETHANONEVLANS_END19:
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
        m[a+n]=rx 4 p1
        ; 1039: address = 58126;
        ; 1040: address += idx*2;
        rx= 58126 r2
        ; 1041: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1042: entry = readFromDevice(address+1,0);
        ; 1043: *out = (int)((entry) & 0x1);
        a= 1
        a+=rx r2
        rx=a r2
        rx= 0 r3
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
RD_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS_END20:
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
        m[a+n]=rx 4 p1
        ; 1047: address = 58126;
        ; 1048: address += idx*2;
        rx= 58126 r2
        ; 1049: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a=a<<1
        a+=rx r2
        rx=a r2
        ; 1050: entry = readFromDevice(address+1,0);
        ; 1051: entry = (entry & ~(int)0x1) | ((int)(dropUntaggedVlans & 0x1));
        a= 1
        a+=rx r2
        rx=a r3
        rx= 0 r4
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
        a= 1
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 1053: }
        a= 1
        a+=rx r2
        rx=a r2
        rx= 0 r4
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_EGRESSPORTCONFIGURATION_DROPUNTAGGEDVLANS_END21:
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
        m[a+n]=rx 4 p1
        ; 1733: address = 56021;
        ; 1734: address += idx*1;
        rx= 56021 r2
        ; 1735: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx= 0 r3
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
RD_HAIRPINENABLE_ALLOWFLOOD_END22:
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
        m[a+n]=rx 4 p1
        ; 1741: address = 56021;
        ; 1742: address += idx*1;
        rx= 56021 r2
        ; 1743: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx=a r3
        rx= 0 r4
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
        a= 1
        a&=rx r5
        a|=rx r4
        rx=a r3
        ; 1747: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_HAIRPINENABLE_ALLOWFLOOD_END23:
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
        m[a+n]=rx 4 p1
        ; 1750: address = 56021;
        ; 1751: address += idx*1;
        rx= 56021 r2
        ; 1752: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        a=a>>1
        rx=a r2
        a= 1
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 4 r3
        a=rx r3
        m[a]=rx r2
RD_HAIRPINENABLE_ALLOWMC_END24:
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
        m[a+n]=rx 4 p1
        ; 1758: address = 56021;
        ; 1759: address += idx*1;
        rx= 56021 r2
        ; 1760: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx=a r3
        rx= 0 r4
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
        a= 1
        a&=rx r5
        rx=a r5
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1764: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_HAIRPINENABLE_ALLOWMC_END25:
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
        m[a+n]=rx 4 p1
        ; 1767: address = 56021;
        ; 1768: address += idx*1;
        rx= 56021 r2
        ; 1769: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
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
RD_HAIRPINENABLE_ALLOWUC_END26:
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
        m[a+n]=rx 4 p1
        ; 1775: address = 56021;
        ; 1776: address += idx*1;
        rx= 56021 r2
        ; 1777: int entry;
        rx=m[a+n] 0 r3
        a=rx r3
        a+=rx r2
        rx=a r2
        rx=a r3
        rx= 0 r4
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
        a= 1
        a&=rx r5
        rx=a r5
        a=a<<1
        a=a<<1
        a|=rx r4
        rx=a r3
        ; 1781: }
        rx= 0 r4
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        a=rx r4
        rx=a p2
        jsr writetodevice
WR_HAIRPINENABLE_ALLOWUC_END27:
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
        m[a+n]=rx 12 p1
        ; 1903: address = 12887;
        ; 1904: address += idx*8;
        rx= 12887 r6
        ; 1905: int i, j;
        rx=m[a+n] 8 r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r9
        ; 1914: }
        rx= 0 r7
FOR35:
        rx= 4 r0
        a=rx r7
        a-=rx r0
        jge endfor37
        ; 1909: int merged = (prev >> 4) | (curr << 28);
        a= 1
        a+=rx r6
        rx=a r1
        a=rx r7
        a+=rx r1
        rx=a r0
        rx= 0 r1
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
        rx= 4 p1
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r0
        a=rx sp
        rx=m[a+n] 4 r1
        a=rx r1
        rx= 28 p1
        rx=a p0
        jsr __shl
        a=rx p0
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        rx=m[a+n] 4 r0
        a=rx r0
        rx=a r9
        ; 1913: }
        rx= 0 r8
FOR38:
        rx= 4 r0
        a=rx r8
        a-=rx r0
        jge endfor40
        ; 1913: }
        a=rx sp
        rx=m[a+n] 0 r0
        a=rx r8
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
        a=rx r7
        a=a<<1
        a=a<<1
        rx=a r2
        a=rx r8
        a+=rx r2
        a+=rx r1
        m[a].b=rx r0
FORCONT39:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for38
ENDFOR40:
FORCONT36:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for35
ENDFOR37:
RD_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END28:
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
        m[a+n]=rx 16 p1
        ; 1917: address = 12887;
        ; 1918: address += idx*8;
        rx= 12887 r6
        ; 1919: int i, j;
        rx=m[a+n] 12 r0
        a=rx r0
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r6
        rx=a r6
        rx=a r0
        rx= 0 r1
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
        rx= 0 r1
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
        rx= 0 r7
FOR54:
        rx= 4 r0
        a=rx r7
        a-=rx r0
        jge endfor56
        ; 1925: for (j = 0; j < 4; j++) {
        rx= 3 r0
        a=rx r7
        a-=rx r0
        jz cmptrue59
        a= 0
        j cmpend60
CMPTRUE59:
        a= 1
CMPEND60:
        rx= 0 r0
        a-=rx r0
        jz ternelse57
        a=rx sp
        rx=m[a+n] 4 r0
        a=rx r0
        j ternend58
TERNELSE57:
        a= 0
TERNEND58:
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        ; 1930: writeToDevice(address+i, wprev, 0);
        rx= 0 r8
FOR61:
        rx= 4 r0
        a=rx r8
        a-=rx r0
        jge endfor63
        ; 1927: wprev |= v << (4 + j*8);
        a=rx sp
        rx=m[a+n] 16 r0
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
        rx=m[a+n] 8 r0
        a=rx r9
        rx=a r1
        rx= 4 r2
        a=rx r8
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
        jge endif65
        ; 1929: }
        a=rx sp
        rx=m[a+n] 0 r0
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
        rx=a p1
        a=rx r1
        rx=a p0
        jsr __shr
        a=rx p0
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
ENDIF65:
FORCONT62:
        a=rx r8
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r8
        a=rx r0
        j for61
ENDFOR63:
        ; 1931: wprev = wcurr;
        a=rx r7
        a+=rx r6
        rx=a r0
        a=rx sp
        rx=m[a+n] 8 r2
        a=rx r2
        rx=a r1
        rx= 0 r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
        ; 1932: }
        a=rx sp
        rx=m[a+n] 0 r0
        m[a+n]=rx 8 r0
FORCONT55:
        a=rx r7
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r7
        a=rx r0
        j for54
ENDFOR56:
        ; 1934: }
        a= 4
        a+=rx r6
        rx=a r0
        a=rx sp
        rx=m[a+n] 8 r2
        a=rx r2
        rx=a r1
        rx= 0 r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
WR_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END41:
        a=rx sp
        rx= 20 r0
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
        ; ======== function memset ========
        ; frame-size: 0, params: 3, max-reg: 8, leaf: yes, vreg: yes, spills: 0
MEMSET:
        push-r r8
        ; 12368: while (n--) *p++ = (unsigned char)c;
        a=rx p0
        rx=a r2
        ; 12369: return s;
WHILE67:
        a=rx p2
        rx=a r3
        rx= -1 r4
        a+=rx r4
        rx=a p2
        a=rx r3
        rx= 0 r5
        a-=rx r5
        jz endwhile68
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
        j while67
ENDWHILE68:
        ; 12370: }
MEMSET_END66:
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
        rx=a p0
        jsr wr_coretickconfiguration_clkdivider
        ; 12384: rd_CoreTickConfiguration_clkDivider(&val);
        rx= 10 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        ; 12385: rd_CoreTickConfiguration_stepDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        ; 12386: wr_CoreTickConfiguration_clkDivider(0x1234 & 0x1ffff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        ; 12387: rd_CoreTickConfiguration_clkDivider(&val);
        rx= 4660 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_clkdivider
        ; 12388: rd_CoreTickConfiguration_stepDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        ; 12389: wr_CoreTickConfiguration_stepDivider(0xf);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        ; 12390: rd_CoreTickConfiguration_stepDivider(&val);
        rx= 15 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        ; 12391: rd_CoreTickConfiguration_clkDivider(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
        ; 12392: wr_CoreTickConfiguration_stepDivider(0xff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickconfiguration_clkdivider
        ; 12393: rd_CoreTickConfiguration_stepDivider(&val);
        rx= 255 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickconfiguration_stepdivider
        ; 12394: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        rx=a p0
        jsr rd_coretickconfiguration_stepdivider
TEST_CORETICKCONFIGURATION_END71:
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
        rx=a p0
        jsr wr_ermredconfiguration_redxoff
        ; 12410: wr_ERMRedConfiguration_redMaxCells(0xf);
        rx= 512 r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxon
        ; 12411: rd_ERMRedConfiguration_redXoff(&val);
        rx= 15 r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redmaxcells
        ; 12412: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        ; 12413: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        ; 12414: wr_ERMRedConfiguration_redXoff(0xabc);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        ; 12415: rd_ERMRedConfiguration_redXoff(&val);
        rx= 2748 r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxoff
        ; 12416: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        ; 12417: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        ; 12418: wr_ERMRedConfiguration_redXon(0x7ff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        ; 12419: rd_ERMRedConfiguration_redXon(&val);
        rx= 2047 r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redxon
        ; 12420: rd_ERMRedConfiguration_redXoff(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
        ; 12421: rd_ERMRedConfiguration_redMaxCells(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        ; 12422: wr_ERMRedConfiguration_redMaxCells(0xaa);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        ; 12423: rd_ERMRedConfiguration_redMaxCells(&val);
        rx= 170 r2
        a=rx r2
        rx=a p0
        jsr wr_ermredconfiguration_redmaxcells
        ; 12424: rd_ERMRedConfiguration_redXoff(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redmaxcells
        ; 12425: rd_ERMRedConfiguration_redXon(&val);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_ermredconfiguration_redxoff
        ; 12426: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        rx=a p0
        jsr rd_ermredconfiguration_redxon
TEST_ERMREDCONFIGURATION_END72:
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
        rx= 0 r2
        rx= 0 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        ; 12431: wr_HairpinEnable_allowUc(0, 1);
        rx= 0 r2
        rx= 0 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowmc
        ; 12432: rd_HairpinEnable_allowFlood(0, &val);
        rx= 0 r2
        rx= 1 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowuc
        ; 12433: rd_HairpinEnable_allowMc(0, &val);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        ; 12434: rd_HairpinEnable_allowUc(0, &val);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowmc
        ; 12435: wr_HairpinEnable_allowFlood(0, 1);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
        ; 12436: rd_HairpinEnable_allowFlood(0, &val);
        rx= 0 r2
        rx= 1 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        ; 12437: rd_HairpinEnable_allowMc(0, &val);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        ; 12438: rd_HairpinEnable_allowUc(0, &val);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowmc
        ; 12439: wr_HairpinEnable_allowFlood(1, 0);
        rx= 0 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
        ; 12440: wr_HairpinEnable_allowMc(1, 0);
        rx= 1 r2
        rx= 0 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        ; 12441: wr_HairpinEnable_allowUc(1, 1);
        rx= 1 r2
        rx= 0 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowmc
        ; 12442: wr_HairpinEnable_allowFlood(0, 0);
        rx= 1 r2
        rx= 1 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowuc
        ; 12443: rd_HairpinEnable_allowFlood(1, &val);
        rx= 0 r2
        rx= 0 r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr wr_hairpinenable_allowflood
        ; 12444: rd_HairpinEnable_allowUc(1, &val);
        rx= 1 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowflood
        ; 12445: }
        rx= 1 r2
        a=rx sp
        rx= 0 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_hairpinenable_allowuc
TEST_HAIRPINENABLE_END73:
        a=rx sp
        rx= 8 r0
        a+=rx r0
        rx=a sp
        pop-r r3
        pop-a
        j-a
        ; ======== function test_EgressPortConfiguration_crossword ========
        ; frame-size: 32, params: 1, max-reg: 5, leaf: no, vreg: yes, spills: 8
TEST_EGRESSPORTCONFIGURATION_CROSSWORD:
        push-srp
        push-r r5
        a=rx sp
        rx= -36 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 32 p0
        ; 12448: int idx = 0;
        ; 12449: wr_EgressPortConfiguration_colorRemap(idx, 0);
        rx= 0 r2
        ; 12450: wr_EgressPortConfiguration_vid(idx, 0);
        ; -- inline begin: result in __inline_result_i1 --
        ; 826: address = 58126;
        ; 827: address += idx*2;
        rx= 58126 r3
        ; 828: int entry;
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        rx=a r4
        rx= 0 r5
        rx=a p0
        a=rx r5
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r4
        ; 831: writeToDevice(address,entry,0);
        a= -2
        a&=rx r4
        rx=a r5
        a= 0
        a|=rx r5
        rx=a r4
        ; 832: }
        rx= 0 r5
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        a=rx r5
        rx=a p2
        jsr writetodevice
INLINE_RET_i1:
        ; -- inline end --
        ; 12451: wr_EgressPortConfiguration_moreThanOneVlans(idx, 0);
        rx= 0 r3
        rx= 0 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_vid
        ; 12452: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 0);
        rx= 0 r3
        rx= 0 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_morethanonevlans
        ; 12453: wr_EgressPortConfiguration_dropSingleTaggedVlans(idx, 0);
        rx= 0 r3
        rx= 0 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_dropuntaggedvlans
        ; 12454: wr_EgressPortConfiguration_moreThanOneVlans(idx, 1);
        ; -- inline begin: result in __inline_result_i2 --
        ; 1064: address = 58126;
        ; 1065: address += idx*2;
        rx= 58126 r3
        a=rx sp
        m[a+n]=rx 24 r3
        rx=m[a+n] 24 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 24 r3
        rx=m[a+n] 24 r3
        a= 1
        a+=rx r3
        rx=a r3
        rx= 0 r4
        rx=a p0
        a=rx r4
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r3
        a=rx sp
        m[a+n]=rx 20 r3
        rx=m[a+n] 20 r3
        a= -3
        a&=rx r3
        rx=a r3
        a= 0
        a|=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 20 r3
        rx=m[a+n] 24 r3
        a= 1
        a+=rx r3
        rx=a r3
        a=rx sp
        rx=m[a+n] 20 r4
        rx= 0 r5
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        a=rx r5
        rx=a p2
        jsr writetodevice
INLINE_RET_i2:
        a=rx sp
        rx=m[a+n] 28 r3
        ; -- inline end --
        ; 12455: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        rx= 0 r3
        rx= 1 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_morethanonevlans
        ; 12456: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        rx= 0 r3
        a=rx sp
        rx= 28 r4
        a+=rx r4
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        ; 12457: wr_EgressPortConfiguration_dropUntaggedVlans(idx, 1);
        rx= 0 r3
        a=rx sp
        rx= 28 r4
        a+=rx r4
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
        ; 12458: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        rx= 0 r3
        rx= 1 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_dropuntaggedvlans
        ; 12459: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        rx= 0 r3
        a=rx sp
        rx= 28 r4
        a+=rx r4
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
        ; 12460: wr_EgressPortConfiguration_vid(idx, 0xabc);
        rx= 0 r3
        a=rx sp
        rx= 28 r4
        a+=rx r4
        rx=a r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        ; 12461: rd_EgressPortConfiguration_vid(idx, &val);
        rx= 0 r3
        rx= 2748 r4
        a=rx r3
        rx=a p0
        a=rx r4
        rx=a p1
        jsr wr_egressportconfiguration_vid
        ; 12462: rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
        ; -- inline begin: result in __inline_result_i3 --
        a=rx sp
        rx= 28 r3
        a+=rx r3
        rx=a r3
        a=rx sp
        m[a+n]=rx 16 r3
        ; 920: address = 58126;
        ; 921: address += idx*2;
        rx= 58126 r3
        m[a+n]=rx 12 r3
        rx=m[a+n] 12 r3
        a=rx r2
        a=a<<1
        a+=rx r3
        rx=a r2
        a=rx sp
        m[a+n]=rx 12 r2
        rx=m[a+n] 12 r2
        a=rx r2
        rx= 0 r3
        rx=a p0
        a=rx r3
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r2
        a=rx sp
        m[a+n]=rx 8 r2
        rx=m[a+n] 8 r2
        a=rx r2
        rx= 12 p1
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r2
        rx= 4095 r3
        a=rx r3
        a&=rx r2
        rx=a r2
        a=rx sp
        rx=m[a+n] 16 r3
        a=rx r3
        m[a]=rx r2
INLINE_RET_i3:
        a=rx sp
        rx=m[a+n] 16 r2
        ; -- inline end --
        ; 12463: rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
        rx= 0 r2
        rx= 28 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_morethanonevlans
        ; 12464: }
        rx= 0 r2
        a=rx sp
        rx= 28 r3
        a+=rx r3
        rx=a r3
        a=rx r2
        rx=a p0
        a=rx r3
        rx=a p1
        jsr rd_egressportconfiguration_dropuntaggedvlans
TEST_EGRESSPORTCONFIGURATION_CROSSWORD_END74:
        a=rx sp
        rx= 36 r0
        a+=rx r0
        rx=a sp
        pop-r r5
        pop-a
        j-a
        ; ======== function test_HashBasedL3RoutingTable_destIPAddr ========
        ; frame-size: 156, params: 1, max-reg: 9, leaf: no, vreg: yes, spills: 39
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR:
        push-srp
        push-r r9
        a=rx sp
        rx= -160 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 156 p0
        ; 12494: unsigned char out[16];
        rx= 0 r6
        ; 12495: unsigned char in[16];
        ; 12496: unsigned char zeros[16] = {0};
        ; 12498: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 124 r1
        a+=rx r1
        rx=a r0
        rx= 0 r1
        rx= 16 r2
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        ; 12499: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 124 r2
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
        rx= 140 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        rx= 0 r9
FOR87:
        rx= 16 r0
        a=rx r9
        a-=rx r0
        jge endfor89
        ; 12501: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a= 1
        a+=rx r9
        mask-a-b
        rx=a r0
        a=rx sp
        rx= 124 r1
        a+=rx r1
        rx=a r1
        a=rx r9
        a+=rx r1
        m[a].b=rx r0
FORCONT88:
        a=rx r9
        rx=a r0
        rx= 1 r1
        a+=rx r1
        rx=a r9
        a=rx r0
        j for87
ENDFOR89:
        ; 12502: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 124 r2
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
        rx= 140 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12504: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        a=rx sp
        rx= 124 r1
        a+=rx r1
        rx=a r0
        rx= 255 r1
        rx= 16 r2
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        ; 12505: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 0 r0
        a=rx sp
        rx= 124 r2
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
        rx= 140 r2
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
        rx= 124 r1
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
        rx= 124 r2
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
        rx= 140 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12510: wr_HashBasedL3RoutingTable_vrf(idx, 1);
        ; -- inline begin: result in __inline_result_i5 --
        ; 1877: address = 12887;
        ; 1878: address += idx*8;
        rx= 12887 r0
        a=rx sp
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
        rx=m[a+n] 56 r1
        a=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 52 r0
        rx=m[a+n] 52 r0
        a= -4
        a&=rx r0
        rx=a r0
        a= 2
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 52 r0
        rx=m[a+n] 56 r1
        a=rx r1
        rx=a r0
        a=rx sp
        rx=m[a+n] 52 r2
        a=rx r2
        rx=a r1
        rx= 0 r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
INLINE_RET_i5:
        a=rx sp
        rx=m[a+n] 60 r0
        ; -- inline end --
        ; 12511: for (int i = 0; i < 16; i++) in[i] = (unsigned char)(0x10 + i);
        ; -- inline begin: result in __inline_result_i6 --
        ; 1894: address = 12887;
        ; 1895: address += idx*8;
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
        rx=m[a+n] 48 r1
        a=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 44 r0
        rx=m[a+n] 44 r0
        rx= -13 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a= 4
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 44 r0
        rx=m[a+n] 48 r1
        a=rx r1
        rx=a r0
        a=rx sp
        rx=m[a+n] 44 r2
        a=rx r2
        rx=a r1
        rx= 0 r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
INLINE_RET_i6:
        a=rx sp
        rx=m[a+n] 52 r0
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
        rx= 124 r1
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
        rx= 124 r2
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
        rx= 140 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
        ; 12515: rd_HashBasedL3RoutingTable_proto(idx, &proto_val);
        ; 12516: rd_HashBasedL3RoutingTable_vrf(idx, &vrf_val);
        ; -- inline begin: result in __inline_result_i7 --
        a=rx sp
        rx= 104 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 40 r0
        ; 1869: address = 12887;
        ; 1870: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 36 r0
        rx=m[a+n] 36 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 36 r0
        rx=m[a+n] 36 r1
        a=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 32 r0
        rx=m[a+n] 32 r0
        a= 3
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 40 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i7:
        a=rx sp
        rx=m[a+n] 40 r0
        ; -- inline end --
        ; 12517: wr_HashBasedL3RoutingTable_nextHopPointer(idx, 0x1aa);
        ; -- inline begin: result in __inline_result_i8 --
        rx= 100 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 28 r0
        ; 1886: address = 12887;
        ; 1887: address += idx*8;
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
        rx=m[a+n] 24 r1
        a=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 20 r0
        rx=m[a+n] 20 r0
        a=rx r0
        a=a>>1
        a=a>>1
        rx=a r0
        a= 3
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 28 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i8:
        a=rx sp
        rx=m[a+n] 28 r0
        ; -- inline end --
        ; 12518: wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
        ; -- inline begin: result in __inline_result_i9 --
        ; 1945: address = 12887;
        ; 1946: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 16 r0
        rx=m[a+n] 16 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 16 r0
        rx=m[a+n] 16 r1
        a= 4
        a+=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 12 r0
        rx=m[a+n] 12 r0
        rx= -8177 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        rx= 6816 r1
        a=rx r1
        a|=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 12 r0
        rx=m[a+n] 16 r1
        a= 4
        a+=rx r1
        rx=a r0
        a=rx sp
        rx=m[a+n] 12 r2
        a=rx r2
        rx=a r1
        rx= 0 r2
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr writetodevice
INLINE_RET_i9:
        a=rx sp
        rx=m[a+n] 20 r0
        ; -- inline end --
        ; 12519: int nhp;
        rx= 0 r0
        rx= 124 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr wr_hashbasedl3routingtable_destipaddr
        ; 12520: rd_HashBasedL3RoutingTable_nextHopPointer(idx, &nhp);
        ; 12521: int idx1 = 1;
        ; -- inline begin: result in __inline_result_i10 --
        a=rx sp
        rx= 96 r0
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 8 r0
        ; 1937: address = 12887;
        ; 1938: address += idx*8;
        rx= 12887 r0
        m[a+n]=rx 4 r0
        rx=m[a+n] 4 r0
        a=rx r6
        a=a<<1
        a=a<<1
        a=a<<1
        a+=rx r0
        rx=a r0
        a=rx sp
        m[a+n]=rx 4 r0
        rx=m[a+n] 4 r1
        a= 4
        a+=rx r1
        rx=a r0
        rx= 0 r1
        rx=a p0
        a=rx r1
        rx=a p1
        jsr readfromdevice
        a=rx p0
        rx=a r0
        a=rx sp
        m[a+n]=rx 0 r0
        rx=m[a+n] 0 r0
        a=rx r0
        rx= 4 p1
        rx=a p0
        jsr __shr
        a=rx p0
        rx=a r0
        rx= 511 r1
        a=rx r1
        a&=rx r0
        rx=a r0
        a=rx sp
        rx=m[a+n] 8 r1
        a=rx r1
        m[a]=rx r0
INLINE_RET_i10:
        a=rx sp
        rx=m[a+n] 8 r0
        ; -- inline end --
        ; 12523: memset(in1, 0xcc, 16);
        ; 12524: wr_HashBasedL3RoutingTable_destIPAddr(idx1, in1);
        rx= 76 r1
        a+=rx r1
        rx=a r0
        rx= 204 r1
        rx= 16 r2
        rx=a p0
        a=rx r1
        rx=a p1
        a=rx r2
        rx=a p2
        jsr memset
        ; 12525: rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
        rx= 1 r0
        a=rx sp
        rx= 76 r2
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
        rx= 140 r2
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
        rx= 60 r2
        a+=rx r2
        rx=a r1
        a=rx r0
        rx=a p0
        a=rx r1
        rx=a p1
        jsr rd_hashbasedl3routingtable_destipaddr
TEST_HASHBASEDL3ROUTINGTABLE_DESTIPADDR_END75:
        a=rx sp
        rx= 160 r0
        a+=rx r0
        rx=a sp
        pop-r r9
        pop-a
        j-a
        ; ======== function main ========
        ; frame-size: 8, params: 1, max-reg: 2, leaf: no, vreg: yes, spills: 2
MAIN:
        push-srp
        push-r r2
        a=rx sp
        rx= -12 r0
        a+=rx r0
        rx=a sp
        a=rx sp
        m[a+n]=rx 8 p0
        ; 12530: test_CoreTickSelect();
        jsr test_coretickconfiguration
        ; 12531: test_ERMRedConfiguration();
        ; -- inline begin: result in __inline_result_i11 --
        ; 12397: wr_CoreTickSelect_clkSelect(1);
        ; 12398: rd_CoreTickSelect_clkSelect(&val);
        rx= 1 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        ; 12399: wr_CoreTickSelect_clkSelect(2);
        a=rx sp
        rx= 4 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        ; 12400: rd_CoreTickSelect_clkSelect(&val);
        rx= 2 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        ; 12401: wr_CoreTickSelect_clkSelect(3);
        a=rx sp
        rx= 4 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        ; 12402: rd_CoreTickSelect_clkSelect(&val);
        rx= 3 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        ; 12403: wr_CoreTickSelect_clkSelect(0xff);
        a=rx sp
        rx= 4 r2
        a+=rx r2
        rx=a p0
        jsr rd_coretickselect_clkselect
        ; 12404: rd_CoreTickSelect_clkSelect(&val);
        rx= 255 r2
        a=rx r2
        rx=a p0
        jsr wr_coretickselect_clkselect
        ; 12405: }
        a=rx sp
        rx= 4 r2
        a+=rx r2
        rx=a r2
        rx=a p0
        jsr rd_coretickselect_clkselect
INLINE_RET_i11:
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
        ; -- inline begin: result in __inline_result_i12 --
        ; 12467: wr_EgressEthernetTypeforVLANtag_typeValue(0xffff);
        ; 12468: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 65535 r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        ; 12469: wr_EgressEthernetTypeforVLANtag_typeValue(0x8100);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        ; 12470: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 33024 r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        ; 12471: wr_EgressEthernetTypeforVLANtag_typeValue(0x88a8);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        ; 12472: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 34984 r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        ; 12473: wr_EgressEthernetTypeforVLANtag_typeValue(0x1ffff);
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
        ; 12474: rd_EgressEthernetTypeforVLANtag_typeValue(&val);
        rx= 131071 r2
        a=rx r2
        rx=a p0
        jsr wr_egressethernettypeforvlantag_typevalue
        ; 12475: }
        a=rx sp
        rx= 0 r2
        a+=rx r2
        rx=a r2
        rx=a p0
        jsr rd_egressethernettypeforvlantag_typevalue
INLINE_RET_i12:
        a=rx r2
        ; -- inline end --
        ; 12536: return 0;
        jsr test_hashbasedl3routingtable_destipaddr
        ; 12537: }
        rx= 0 p0
MAIN_END98:
        a=rx sp
        rx= 12 r0
        a+=rx r0
        rx=a sp
        pop-r r2
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
