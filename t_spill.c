volatile    int a = 1, b = 2, c = 3, d = 4;
volatile    int e = 5, f = 6, g = 7, h = 8;
volatile    int i = 9, j = 10, k = 11, l = 12;
volatile    int m = 13, n = 14, o = 15, p = 16;
volatile    int q = 17, r = 18;
int main() {
    // This function uses more than 16 variables to force spilling.

    int result = a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r;

    return result;
}
