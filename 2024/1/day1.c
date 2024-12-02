#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

int do_cmp(const void *vx, const void *vy) {
    int x = *(int *)vx;
    int y = *(int *)vy;
    return x < y;
}

int main() {
    FILE *inp = fopen("input.txt", "r");
    fseek(inp, 0, SEEK_END);
    long end = ftell(inp);
    fseek(inp, 0, SEEK_SET);
    char *buf = calloc(end + 1, sizeof(char));
    fread(buf, sizeof(char), end, inp);

    int *left = calloc(end + 1, sizeof(int));
    int *right = calloc(end + 1, sizeof(int));
    char *res = strtok(buf, " \n");
    int len = 0;
    while (1) {
        left[len] = atoi(res);
        res = strtok(NULL, " \n");
        right[len] = atoi(res);
        res = strtok(NULL, " \n");
        len += 1;
        if (!res) break;
    }

    qsort(left, len, sizeof(int), do_cmp);
    qsort(right, len, sizeof(int), do_cmp);

    int diffsum = 0;
    for (int i = 0; i < len; ++i) {
        diffsum += abs(left[i] - right[i]);
    }
    printf("%d\n", diffsum);

    int simsum = 0;
    for (int i = 0; i < len; ++i) {
        int occurs = 0;
        for (int j = 0; j < len; ++j) {
            if (right[j] == left[i]) occurs += 1;
        }
        simsum += left[i] * occurs;
    }
    printf("%d\n", simsum);
}
