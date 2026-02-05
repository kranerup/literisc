// Final attempt to force a spill.
// The idea is to make many variables live for the entire duration of the function.
int main() {
    int v0 = 1;
    int v1 = 2;
    int v2 = 3;
    int v3 = 4;
    int v4 = 5;
    int v5 = 6;
    int v6 = 7;
    int v7 = 8;
    int v8 = 9;
    int v9 = 10;
    int v10 = 11;
    int v11 = 12;
    int v12 = 13;
    int v13 = 14;
    int v14 = 15;
    int v15 = 16;
    int v16 = 17;
    int v17 = 18;
    int v18 = 19;
    int v19 = 20;

    // By mixing variables from the beginning and end of the declaration list,
    // we force their live intervals to span the whole function.
    int r0 = v0 + v19;
    int r1 = v1 + v18;
    int r2 = v2 + v17;
    int r3 = v3 + v16;
    int r4 = v4 + v15;
    int r5 = v5 + v14;
    int r6 = v6 + v13;
    int r7 = v7 + v12;
    int r8 = v8 + v11;
    int r9 = v9 + v10;

    int final_result = r0 + r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9;
    return final_result;
}
