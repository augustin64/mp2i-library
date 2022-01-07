#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int somme(int tab[], int n) {
    // printf("%d, %d\n", sizeof(tab), sizeof(int));
    int s = 0;
    for (int i=0; i < n; i++) {
        s += tab[i];
    };
    return s;
};

void swap(int* tab, int i, int j) {
    int tmp = tab[i];
    tab[i] = tab[j];
    tab[j] = tmp;
};

void reverse(int* tab, int n) {
    for (int i=0; i < n/2; i++) {
        swap(tab, i, n-i-1);
    };
};

double* copy(double t[], int n) {
    double* t2 = (double*)malloc(n * sizeof(double));
    for (int i=0; i < n; i++) {
        t2[i] = t[i];
    };
    return t2;
};
