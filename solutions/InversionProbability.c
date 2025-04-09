// https://cses.fi/problemset/task/1728/

#include <stdio.h>
#include <stdlib.h>

long double prob_sum = 0.0L;

long double fabsl(long double x) {
    return x < 0.0L ? -x : x;
}
long double roundl(long double x) {
    long double intpart = (long double)((long long)x);
    long double frac = x - intpart;
    if (fabsl(frac) < 0.5L) return intpart;
    if (fabsl(frac) > 0.5L) return x > 0 ? intpart + 1.0L : intpart - 1.0L;
    return ((long long)intpart % 2 == 0) ? intpart : (x > 0 ? intpart + 1.0L : intpart - 1.0L);
}
long double fmodl(long double x, long double y) {
    if (y == 0.0L) return 0.0L;
    long double div = x / y;
    long double intpart = (long double)((long long)div);
    return x - intpart * y;
}
int main() {
    int count;
    if (scanf("%d", &count) != 1 || count < 0) return 1;
    if (count == 0) {
        printf("%.6Lf\n", 0.0L);
        return 0;
    }
    long long *r_vals = malloc((size_t)count * sizeof(long long));
    if (!r_vals && count > 0) return 1;
    for (int i = 0; i < count; ++i) {
        if (scanf("%lld", &r_vals[i]) != 1 || r_vals[i] <= 0) {
            free(r_vals);
            return 1;
        }
    }
    for (int i = 0; i < count; ++i) {
        long long r1 = r_vals[i];
        if (r1 == 1) continue;
        for (int j = i + 1; j < count; ++j) {
            long long r2 = r_vals[j];
            long double favorable;
            long double total;
            if (r1 <= r2) {
                total = 2.0L * r2;
                favorable = (long double)r1 - 1.0L;
            } else {
                total = 2.0L * r1;
                favorable = (long double)(2 * r1 - r2 - 1);
            }
            if (total > 0.0L) {
                 prob_sum += (favorable / total) * 1.0e9L;
            }
        }
    }
    long double intermediate_val = prob_sum / 1.0e3L;
    long double rounded_val_even;
    long double temp_rounded = roundl(intermediate_val);
    long double round_diff = temp_rounded - intermediate_val;
    if (fabsl(round_diff) != 0.5L) {
        rounded_val_even = temp_rounded;
    } else {
        if (fmodl(temp_rounded, 2.0L) == 0.0L) {
            rounded_val_even = temp_rounded;
        } else {
            rounded_val_even = intermediate_val - round_diff;
        }
    }
    long double final_result = rounded_val_even / 1.0e6L;
    printf("%.6Lf\n", final_result);
    free(r_vals);
    return 0;
}