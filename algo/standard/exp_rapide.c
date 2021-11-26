#include <stdio.h>
#include <stdlib.h>

int exp_rapide(int a, unsigned n) {
    if (n==1) {
        return a;
    };
    int a_n = exp_rapide(a, n/2);
    if (n % 2 == 0) {
        return a_n * a_n;
    };
    return a_n * a_n * a;
};

void tests() {
    int a=5;
    unsigned n=2;
    printf("%d^%d = %d\n", a, n, exp_rapide(a, n));
}

int main() {
    // tests();
    return 0;
}
