#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

unsigned singleton(unsigned);
unsigned union2(unsigned, unsigned);
bool has(unsigned, unsigned);
unsigned card(unsigned);

unsigned singleton(unsigned i) {
      return 1 << i;
};

unsigned union2(unsigned s1, unsigned s2) {
    return s1 & s2;
};

bool has(unsigned s, unsigned e) {
    return union2(s, singleton(e)) != 0;
};

unsigned card(unsigned s) {
    unsigned c = 0;
    while (s != 0) {
        c++;
        s = s & (s-1);
    };
    return c;
};

void tests() {
    printf("singleton de 4: \t%d\n", singleton(4));
    printf("union {0,1} et {1,2}: \t%d\n", union2(3, 6));
    printf("{2, 4} possède 4: \t%d\n", has(20, 4));
    printf("cardinal de 17: \t%d\n", card(17));
}

int main() {
    // tests();
    return 0;
};
