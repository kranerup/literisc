// Final attempt to force a spill.
volatile    int v0 = 1;
volatile    int v1 = 2;
volatile    int v2 = 3;
volatile    int v3 = 4;
volatile    int v4 = 5;
volatile    int v5 = 6;
volatile    int v6 = 7;
volatile    int v7 = 8;
volatile    int v8 = 9;
volatile    int v9 = 10;
volatile    int v10 = 11;
volatile    int v11 = 12;
volatile    int v12 = 13;
volatile    int v13 = 14;
volatile    int v14 = 15;
volatile    int v15 = 16;
volatile    int v16 = 17;
volatile    int v17 = 18;
volatile    int v18 = 19;
volatile    int v19 = 20;
// The idea is to make many variables live for the entire duration of the function.
int main() {

    // By mixing variables from the beginning and end of the declaration list,
    // we force their live intervals to span the whole function.
    int x0 = v0 + v19;
    int x1 = v1 + v18;
    int x2 = v2 + v17;
    int x3 = v3 + v16;
    int x4 = v4 + v15;
    int x5 = v5 + v14;
    int x6 = v6 + v13;
    int x7 = v7 + v12;
    int x8 = v8 + v11;
    int x9 = v9 + v10;

    int final_result = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9;
    return final_result;
}
