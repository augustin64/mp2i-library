#include <stdio.h>
#include <stdlib.h>

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
};

int main() {
    tests();
    return 0;
}
