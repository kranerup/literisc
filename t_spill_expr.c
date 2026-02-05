// This function has a deeply nested expression that should require
// many temporary registers to evaluate, hopefully forcing a spill.
int func(int a, int b, int c, int d, int e, int f) {
    return (a + b) * (c + d) * (e + f) +
           (a - c) * (b - d) * (e - f) +
           (a * c) * (b * d) * (e * f) +
           ((a+1)*(b+2)*(c+3)*(d+4)*(e+5)*(f+6)) +
           ((a-1)*(b-2)*(c-3)*(d-4)*(e-5)*(f-6)) +
           ((a*2)/(b+1) + (c*3)/(d+2) + (e*4)/(f+3));
}

int main() {
    return func(10, 20, 30, 40, 50, 60);
}
