#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

/* DÉFINITION DE LA STRUCTURE DE LISTE CHAÎNÉE */

typedef struct list list;

struct list {
    list* next;
    int elem;
};

list* add(list* l, int e) {
    list* l_new = (list*)malloc(sizeof(int)+sizeof(list*));
    l_new->next = l;
    l_new->elem = e;
    return l_new;
};

list* range(int n) {
    list* l = NULL;
    for (int i=n-1; i>=0; i--) {
        l = add(l, i);
    };
    return l;
};

void print_list(list* l) {
    printf("[");
    while (l != NULL) {
        printf("%d, ", l->elem);
        l = l->next;
    };
    printf("]\n");
};

bool has(list *l, int e) {
    while (l != NULL) {
        if (l->elem==e) {
            return true;
        };
        l=l->next;
    };
    return false;
};

int size(list* l) {
    int n = 0;
    while (l!= NULL) {
        n++;
        l = l->next;
    };
    return n;
};

list* reverse(list* l) {
    list* l2 = NULL;
    while (l!=NULL) {
        l2 = add(l2, l->elem);
        l = l->next;
    }
    return l2;
};


void tests_ds() {
    list* l = NULL;
    list* l2 = add(l, 2);
    printf("liste: %d\n", l2->elem);
    print_list(range(500));
    print_list(l2);
    printf("has(l2, 5): %d, has(range, 3): %d\n", has(l2, 5), has(range(5), 1));
    printf("len(l2): %d, len(range10): %d\n", size(l2), size(range(10)));
    print_list(reverse(range(10)));
    free(l2);
};


/* FONCTIONS OPÉRANT SUR LES LISTES CHAÎNÉES */

bool croissante(list* l) {
    bool val = true;
    while (l->next!=NULL) {
        val = val && l->elem <= l->next->elem;
        l= l->next;
    };
    return val;
};

list* insere(list* l, int e) {
    if (l==NULL) {
        return add(l, e);
    };
    if (l->elem>e) {
        return add(l, e);
    };
    return add(insere(l->next, e), l->elem);
};

list* tri_insertion(list* l) {
    list* lt = NULL;
    while (l!=NULL) {
        lt = insere(lt, l->elem);
        l = l->next;
    };
    return lt;
};


void tests_algos() {
    printf("range croissante: %d\n", croissante(range(5)));
    printf("range decroissance: %d\n", croissante(reverse(range(5))));

    list* l = range(10);
    print_list(insere(l, 5));

    int tab[] = {0, 2, 5, 1, 7 ,2, 8};
    list* l2 = NULL;
    for (int i=0; i<7; i++) {
        l2 = add(l2, tab[i]);
    }
    print_list(l2);
    print_list(tri_insertion(l2));
};

int main() {
    // tests_ds();
    // tests_algos();
    return 0;
};
