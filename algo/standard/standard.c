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

int factoriel(int n) {
    int fact = 1;
    for (int i=1; i<=n; i++) {
        fact *= i;
    };
    return fact;
};

int rec_factoriel(int n) {
    if (n==1) {
        return 1;
    } else {
        return rec_factoriel(n-1) * n;
    };
};

void hanoi(int n, int i, int j) {
    int k = 3 - i - j;
    if (n!=0) {
        hanoi(n-1, i, k);
        printf("Déplace %d vers %d\n", i, j);
        hanoi(n-1, k, j);
    };
};

void tests() {
    hanoi(3, 0, 2);

    printf("4!: %d \n", factoriel(4));
    printf("4!: %d \n", rec_factoriel(4));

    int a=5;
    unsigned n=2;
    printf("%d^%d = %d\n", a, n, exp_rapide(a, n));
}

int main() {
    tests();
    return 0;
}
