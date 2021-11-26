#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool dicho(int* t, int e, unsigned n) {
    int i = 0;
    int j = n-1;
    int m;
    while (i<=j) {
        m = (i+j)/2;
        if (e == t[m]) {
            return true;
        };
        if (e < t[m]) {
            j = m-1;
        };
        if (e > t[m]) {
            i = m+1;
        };
    };
    return false;
};


void tests() {
    int tab[] = {1, 4, 6, 8, 10, 35};
    printf("false: %d, true: %d\n", dicho(tab, 3, 6), dicho(tab, 35, 6));
};

int main() {
    // tests();
    return 0;
}
