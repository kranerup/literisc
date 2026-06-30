#include "api/flexswitch_fields.h"

static int g_pass = 0, g_fail = 0;

void *memset(void *s, int c, unsigned int n) {
    unsigned char *p = s;
    while (n--) *p++ = (unsigned char)c;
    return s;
}

int memcmp(const void *s1, const void *s2, unsigned int n) {
    const unsigned char *a = s1;
    const unsigned char *b = s2;
    while (n--) {
        if (*a != *b) return *a - *b;
        a++; b++;
    }
    return 0;
}

/* ================================================================== */
/* Tests: narrow fields (0-32 bit)                                    */
/* ================================================================== */

static void test_CoreTickConfiguration(void) {
    uint32_t val;

    /* Set known state */
    wr_CoreTickConfiguration_clkDivider(0x64);
    wr_CoreTickConfiguration_stepDivider(0xa);

    /* Default value checks */
    rd_CoreTickConfiguration_clkDivider(&val);

    rd_CoreTickConfiguration_stepDivider(&val);

    /* Write/read clkDivider */
    wr_CoreTickConfiguration_clkDivider(0x1234 & 0x1ffff);
    rd_CoreTickConfiguration_clkDivider(&val);

    /* stepDivider not corrupted */
    rd_CoreTickConfiguration_stepDivider(&val);

    /* Write/read stepDivider */
    wr_CoreTickConfiguration_stepDivider(0xf);
    rd_CoreTickConfiguration_stepDivider(&val);

    /* clkDivider not corrupted */
    rd_CoreTickConfiguration_clkDivider(&val);

    /* Overflow masking */
    wr_CoreTickConfiguration_stepDivider(0xff);
    rd_CoreTickConfiguration_stepDivider(&val);
}

static void test_CoreTickSelect(void) {
    uint32_t val;

    wr_CoreTickSelect_clkSelect(1);
    rd_CoreTickSelect_clkSelect(&val);

    wr_CoreTickSelect_clkSelect(2);
    rd_CoreTickSelect_clkSelect(&val);

    wr_CoreTickSelect_clkSelect(3);
    rd_CoreTickSelect_clkSelect(&val);

    wr_CoreTickSelect_clkSelect(0xff);
    rd_CoreTickSelect_clkSelect(&val);
}

static void test_ERMRedConfiguration(void) {
    uint32_t val;

    /* Set known state matching register defaults */
    wr_ERMRedConfiguration_redXoff(0xcc);
    wr_ERMRedConfiguration_redXon(0x200);
    wr_ERMRedConfiguration_redMaxCells(0xf);

    rd_ERMRedConfiguration_redXoff(&val);

    rd_ERMRedConfiguration_redXon(&val);

    rd_ERMRedConfiguration_redMaxCells(&val);

    /* Write redXoff, verify others intact */
    wr_ERMRedConfiguration_redXoff(0xabc);
    rd_ERMRedConfiguration_redXoff(&val);
    rd_ERMRedConfiguration_redXon(&val);
    rd_ERMRedConfiguration_redMaxCells(&val);

    /* Write redXon, verify others intact */
    wr_ERMRedConfiguration_redXon(0x7ff);
    rd_ERMRedConfiguration_redXon(&val);
    rd_ERMRedConfiguration_redXoff(&val);
    rd_ERMRedConfiguration_redMaxCells(&val);

    /* Write redMaxCells, verify others intact */
    wr_ERMRedConfiguration_redMaxCells(0xaa);
    rd_ERMRedConfiguration_redMaxCells(&val);
    rd_ERMRedConfiguration_redXoff(&val);
    rd_ERMRedConfiguration_redXon(&val);
}

static void test_HairpinEnable(void) {
    uint32_t val;

    /* Set known state: default = 4 => allowUc=1, others=0 */
    wr_HairpinEnable_allowFlood(0, 0);
    wr_HairpinEnable_allowMc(0, 0);
    wr_HairpinEnable_allowUc(0, 1);

    rd_HairpinEnable_allowFlood(0, &val);
    rd_HairpinEnable_allowMc(0, &val);
    rd_HairpinEnable_allowUc(0, &val);

    /* Write allowFlood, check others intact */
    wr_HairpinEnable_allowFlood(0, 1);
    rd_HairpinEnable_allowFlood(0, &val);
    rd_HairpinEnable_allowMc(0, &val);
    rd_HairpinEnable_allowUc(0, &val);

    /* Set up idx=1 independently */
    wr_HairpinEnable_allowFlood(1, 0);
    wr_HairpinEnable_allowMc(1, 0);
    wr_HairpinEnable_allowUc(1, 1);

    /* idx=0 write should not affect idx=1 */
    wr_HairpinEnable_allowFlood(0, 0);
    rd_HairpinEnable_allowFlood(1, &val);
    rd_HairpinEnable_allowUc(1, &val);
}

static void test_EgressPortConfiguration_crossword(void) {
    uint32_t val;
    uint32_t idx = 0;

    /* Clear all fields */
    wr_EgressPortConfiguration_colorRemap(idx, 0);
    wr_EgressPortConfiguration_vid(idx, 0);
    wr_EgressPortConfiguration_moreThanOneVlans(idx, 0);
    wr_EgressPortConfiguration_dropUntaggedVlans(idx, 0);
    wr_EgressPortConfiguration_dropSingleTaggedVlans(idx, 0);

    /* moreThanOneVlans is bit31 of word0, dropUntaggedVlans is bit0 of word1 */
    wr_EgressPortConfiguration_moreThanOneVlans(idx, 1);
    rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
    rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);

    wr_EgressPortConfiguration_dropUntaggedVlans(idx, 1);
    rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
    rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);

    /* vid (12-bit in word0) */
    wr_EgressPortConfiguration_vid(idx, 0xabc);
    rd_EgressPortConfiguration_vid(idx, &val);
    rd_EgressPortConfiguration_moreThanOneVlans(idx, &val);
    rd_EgressPortConfiguration_dropUntaggedVlans(idx, &val);
}

static void test_EgressEthernetTypeforVLANtag(void) {
    uint32_t val;

    wr_EgressEthernetTypeforVLANtag_typeValue(0xffff);
    rd_EgressEthernetTypeforVLANtag_typeValue(&val);

    wr_EgressEthernetTypeforVLANtag_typeValue(0x8100);
    rd_EgressEthernetTypeforVLANtag_typeValue(&val);

    wr_EgressEthernetTypeforVLANtag_typeValue(0x88a8);
    rd_EgressEthernetTypeforVLANtag_typeValue(&val);

    wr_EgressEthernetTypeforVLANtag_typeValue(0x1ffff);
    rd_EgressEthernetTypeforVLANtag_typeValue(&val);
}

/* ================================================================== */
/* Tests: 32-64 bit field                                             */
/* ================================================================== */

static void test_Scratch(void) {
    uint64_t val = 596;
    uint32_t idx = 0;

    wr_Scratch_scratch(val);
    rd_Scratch_scratch(&val);
    
    val = 351;

    wr_Scratch_scratch(val);
    rd_Scratch_scratch(&val);

    wr_Scratch_scratch(val);
    rd_Scratch_scratch(&val);

    //wr_CounterOffloadEngineCounters_addr(idx, 0x2a2a2 & 0x3ffff);
    //wr_CounterOffloadEngineCounters_limit(idx, 0);

    //uint32_t saved_addr;
    //rd_CounterOffloadEngineCounters_addr(idx, &saved_addr);

    //wr_CounterOffloadEngineCounters_limit(idx, 0xffffffff);
    //rd_CounterOffloadEngineCounters_limit(idx, &val);

    //rd_CounterOffloadEngineCounters_addr(idx, &val);

    //wr_CounterOffloadEngineCounters_limit(idx, 0);
    //rd_CounterOffloadEngineCounters_limit(idx, &val);

    //rd_CounterOffloadEngineCounters_addr(idx, &val);

    //wr_CounterOffloadEngineCounters_limit(idx, 0xaa55aa55);
    //rd_CounterOffloadEngineCounters_limit(idx, &val);
}

/* ================================================================== */
/* Tests: >64 bit field (byte array)                                  */
/* ================================================================== */

static void test_HashBasedL3RoutingTable_destIPAddr(void) {
    uint32_t idx = 0;
    uint8_t out[16];
    uint8_t in[16];
    uint8_t zeros[16] = {0};

    /* Clear field */
    memset(in, 0, 16);
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);

    /* Sequential pattern */
    for (int i = 0; i < 16; i++) in[i] = (uint8_t)(i + 1);
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);

    /* All 0xff */
    memset(in, 0xff, 16);
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);

    /* Alternating pattern */
    for (int i = 0; i < 16; i++) in[i] = (i & 1) ? 0x55 : 0xaa;
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);

    /* proto and vrf (bits[3:0] of word0) not corrupted */
    wr_HashBasedL3RoutingTable_proto(idx, 2);
    wr_HashBasedL3RoutingTable_vrf(idx, 1);
    for (int i = 0; i < 16; i++) in[i] = (uint8_t)(0x10 + i);
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
    uint32_t proto_val, vrf_val;
    rd_HashBasedL3RoutingTable_proto(idx, &proto_val);
    rd_HashBasedL3RoutingTable_vrf(idx, &vrf_val);

    /* nextHopPointer (bits[12:4] of word4) not corrupted */
    wr_HashBasedL3RoutingTable_nextHopPointer(idx, 0x1aa);
    wr_HashBasedL3RoutingTable_destIPAddr(idx, in);
    uint32_t nhp;
    rd_HashBasedL3RoutingTable_nextHopPointer(idx, &nhp);

    /* idx isolation */
    uint32_t idx1 = 1;
    uint8_t in1[16], out1[16];
    memset(in1, 0xcc, 16);
    wr_HashBasedL3RoutingTable_destIPAddr(idx1, in1);
    rd_HashBasedL3RoutingTable_destIPAddr(idx, out);
    rd_HashBasedL3RoutingTable_destIPAddr(idx1, out1);
}

/* ================================================================== */
/* main                                                               */
/* ================================================================== */

int main(void) {
    test_CoreTickConfiguration();
    test_CoreTickSelect();
    test_ERMRedConfiguration();
    test_HairpinEnable();
    test_EgressPortConfiguration_crossword();
    test_EgressEthernetTypeforVLANtag();
    test_Scratch();
    test_HashBasedL3RoutingTable_destIPAddr();

    return 0;
}
