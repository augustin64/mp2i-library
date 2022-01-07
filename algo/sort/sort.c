#include <stdio.h>
#include <stdlib.h>

int maximum(int* t, int n) {
    int m = t[0];
    for (int i=0; i<n; i++) {
        m = m < t[i] ? t[i] : m;
    };
    return m;
};


void tri_comptage(int* t, int n) {
    int m = maximum(t, n);
    int* cpt = (int*)malloc(sizeof(int)*n);
    for (int i=0; i<n; i++) {
        cpt[t[i]]++;
    };
    int k = 0;
    for (int i=0; i<m; i++) {
        for (int j=1; j<cpt[i]; j++) {
            t[k] = i;
            k++;
            printf("%d %d %d\n", k, j, cpt[i]);
        };
    };
    free(cpt);
};

void tests() {
    int tab[] = {0, 1, 5 ,1, 76, 2};
    printf("max: %d\n", maximum(tab, 6));

    tri_comptage(tab, 6);
};

int main() {
    tests();
    return 0;
}
