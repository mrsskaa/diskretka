#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

unsigned long long int base = (1ULL << (8 * sizeof(int)));

typedef struct{
    int d;
    unsigned int* a;
} LongNumber;

LongNumber* init(long long int number){
    LongNumber* digit = (LongNumber*) malloc(sizeof(LongNumber));

    if(number == 0){
        digit->a = (unsigned int*) malloc(2 * sizeof(unsigned int));
        if(digit->a == NULL){
            free(digit);
            return NULL;
        }
        digit->a[0] = 1;
        digit->a[1] = 0;
        digit->d = 0;
        return digit;
    }

    unsigned int cnt = 0, i = 0;
    unsigned long long int n = fabs(number);
    while(n>0){
        cnt++;
        n /= base;
    }
    n = fabs(number)    ;
    digit->a = (unsigned  int*) malloc((cnt+1) * sizeof(unsigned int));
    if(digit->a == NULL){
            free(digit);
            return NULL;
    }

    while(n>0){
        i++;
        digit->a[i] = n%base;
        n /= base;
    }
    digit->a[0] = cnt;

    if(number<0){
        digit->d = digit->a[cnt] | 0x80000000;
    }
    else{
        digit->d = digit->a[cnt];
    }

    return digit;
}

void dinit(LongNumber* digit){
    if(digit != NULL){
        if(digit->a != NULL){
            free(digit->a);
        }
        free(digit);
    }
}

LongNumber* normalize(LongNumber* x){
    for(unsigned int i = x->a[0]; i>0; i--){
        if(x->a[i] == 0){
            x->a[0]--;
        }
        else{
            break;
        }
    }
    unsigned int* old = x->a;
    x->a = realloc(x->a, (x->a[0]+1)*sizeof(unsigned int));

    if(!x->a){
        printf("Error");
        x->a = old;
        return NULL;
    }

    return x;
}

LongNumber* copy(LongNumber* x){
    LongNumber* p = (LongNumber*) malloc(sizeof(LongNumber));
    if(!p){
        printf("Error");
        return NULL;
    }

    p->d = x->d;
    p->a = (unsigned int*) malloc(sizeof(unsigned int)*(x->a[0]+1));
    if(!p->a){
        printf("Error");
        dinit(p);
        return NULL;
    }

    memcpy(p->a, x->a, (x->a[0]+1)*sizeof(unsigned int));

    return p;
}


LongNumber* delete_from_first(LongNumber* X, LongNumber*Y){
    LongNumber* x = NULL;
    LongNumber* y = NULL;
    unsigned int sign = 0;

    int cmp = compare(X, Y);
    if(cmp == 1){
        x = copy(X);
        if(!x) return NULL;
        y = copy(Y);
        if(!y){
            dinit(x);
            return NULL;
        }
        sign = 0;
    }
    else if(cmp == 0){
        return init(0);
    }
    else{
        x = copy(Y);
        y = copy(X);
        sign = 1;
    }

    for(unsigned int i = 1; i<=x->a[0]; i++){
        if(y->a[0]<=i) break;
        if(x->a[i]>=y->a[i]){
            x->a[i] -= y->a[i];
        }
        else{
            unsigned int j = i+1;
            while(j <= x->a[0] && x->a[j]==0){
                x->a[j] = base-1;
                j++;
            }
            if(j <= x->a[0]){
                x->a[j]--;
            }
            x->a[i] = x->a[i]+base - y->a[i];
        }
    }

    if(sign==1){
        x->d = x->a[x->a[0]] | 0x80000000;
    }
    else{
        x->d = x->a[x->a[0]];
    }
    dinit(y);
    return normalize(x);
}

LongNumber* add_to_first(LongNumber* X, LongNumber* Y){
    int sign_x = (X->d >> 31) & 1, sign_y = (Y->d >> 31) & 1;

    LongNumber* x = NULL;
    LongNumber* y = NULL;

    if(compare(X, Y) >= 0){
        x = copy(X);
        if(!x) return NULL;
        y = copy(Y);
        if(!y){
            dinit(x);
            return NULL;
}
    }
    else{
        x = copy(Y);
        if(!x) return NULL;
        y = copy(X);
        if(!y){
            dinit(x);
            return NULL;
        }
    }

    if(sign_x == sign_y){
        unsigned int* new_a = realloc(x->a, (x->a[0] + 2) * sizeof(unsigned int));

        if(!new_a) {
            printf("Error");
            return NULL;
        }

        x->a = new_a;
        x->a[0]++;
        x->a[x->a[0]] = 0;
        unsigned int perenos = 0;
        for(unsigned int i = 1; i<=x->a[0]; i++){
            if(i>y->a[0]){
                if(x->a[i]+perenos >= base){
                    x->a[i] = (x->a[i]+perenos)%base;
                    perenos = (x->a[i]+perenos)/base;
                }
                else{
                    x->a[i] = x->a[i]+perenos;
                    perenos = 0;
                }
            }
            else{
                unsigned long long int sum = (unsigned long long) x->a[i] + y->a[i];
                if(sum+perenos >= base){
                    x->a[i] = (sum+perenos)%base;
                    perenos = (sum+perenos) / base;
                }
                else{
                    x->a[i] = sum+perenos;
                    perenos = 0;
                }
            }
        }
        dinit(y);
        return normalize(x);
    }
    else{
        if(sign_x == 1){
            return delete_from_first(x, y);
        }
        else{
            return delete_from_first(y, x);
        }
    }
}

LongNumber* multiply_by_digit(LongNumber* x, unsigned int digit){
    LongNumber* p = copy(x);
    if(!p){
        printf("Error");
        return NULL;
    }

    unsigned int perenos = 0;

    for(unsigned int i = 1; i <= p->a[0]; i++){
        unsigned int current = p->a[i];

        unsigned long long prod = (unsigned long long)current * digit + perenos;
        p->a[i] = prod % base;
        perenos = prod / base;
    }

    if(perenos > 0){
        unsigned int* new_a = realloc(p->a, (p->a[0] + 2) * sizeof(unsigned int));
        if(new_a){
            p->a = new_a;
            p->a[0]++;
            p->a[p->a[0]] = perenos;
        }
        else{
            printf("Error");
            return NULL;
        }
    }

    return normalize(p);
}

LongNumber* multiply(LongNumber* x, LongNumber* y){
    LongNumber* x_copy = copy(x);
    LongNumber* y_copy = copy(y);

    if(!x_copy){
        printf("Error");
        return NULL;
    }

    if(!y_copy){
        printf("Error");
        dinit(x_copy);
        return NULL;
    }

    if((x_copy->a[0] == 1 && x_copy->a[1] == 0) || (y_copy->a[0] == 1 && y_copy->a[1] == 0)){
        dinit(y_copy);
        return x_copy;
    }

    int sign_x = (x_copy->d >> 31) & 1;
    int sign_y = (y_copy->d >> 31) & 1;
    int sign_res = sign_x ^ sign_y;

    x_copy->d &= 0x7FFFFFFF;
    y_copy->d &= 0x7FFFFFFF;


    LongNumber* ans = init(0);
    for(unsigned int i = 1; i<=y_copy->a[0]; i++){
        LongNumber* p = multiply_by_digit(x_copy, y_copy->a[i]);
        if(i > 1){
            unsigned int old_len = p->a[0];
            unsigned int new_len = old_len + (i - 1);

            unsigned int* new_a = realloc(p->a, (new_len + 1) * sizeof(unsigned int));
            if(new_a){
                p->a = new_a;

                for(unsigned int k = old_len; k >= 1; k--){
                    p->a[k + i - 1] = p->a[k];
                }

                for(unsigned int k = 1; k <= i-1; k++){
                    p->a[k] = 0;
                }

                p->a[0] = new_len;
            }
            else{
                printf("Error");
                dinit(p);
                dinit(ans);
                dinit(x_copy);
                dinit(y_copy);
                return NULL;
            }
        }
        add_to_first(ans, p);

        if(!ans){
            printf("Error");
            return NULL;
        }
        dinit(p);
    }
    if(sign_res){
        ans->d |= 0x80000000;
    }

    dinit(x_copy);
    dinit(y_copy);

    return normalize(ans);
}

LongNumber* for_popolam(LongNumber* num){
    LongNumber* x = copy(num);
    if(!x) return NULL;

    unsigned int cnt = x->a[0];
    if((cnt%2) != 0){
        unsigned int* new_x = realloc(x->a, (x->a[0]+1)*sizeof(unsigned int));
        if(new_x){
            x->a = new_x;
            x->a[0]++;
            cnt++;
        }
        else{
            printf("Error");
            return NULL;
        }

        for(unsigned int i = cnt; i>1; i--){
            x->a[i] = x->a[i-1];
        }
        x->a[1] = 0;
    }
    return x;
}

LongNumber* firstPart(LongNumber* num){
    LongNumber* x = for_popolam(num);
    if(!x) return NULL;

    unsigned int cnt = x->a[0];

    LongNumber* x0 = (LongNumber*) malloc(sizeof(LongNumber));
    if(!x0) {
    dinit(x);
    return NULL;
}

    x0->a = (unsigned int*) malloc(sizeof(unsigned int)* (cnt/2+1));
    if(!x0->a) {
    free(x0);
    dinit(x);
    return NULL;
    }

    x0->a[0] = cnt/2;

    for(unsigned int i = 1; i<cnt/2 +1; i++){
        x0->a[i] = x->a[i];
    }

    dinit(x);
    return x0;
}

LongNumber* secondPart(LongNumber* num){
    LongNumber* x = for_popolam(num);
    if(!x) return NULL;

    unsigned int cnt = x->a[0];

    LongNumber* x1 = (LongNumber*) malloc(sizeof(LongNumber));
    if(!x1) {
        dinit(x);
        return NULL;
    }

    x1->a = (unsigned int*) malloc(sizeof(unsigned int)* (cnt/2+1));
    if(!x1->a) {
        free(x1);
        dinit(x);
        return NULL;
    }
    x1->a[0] = cnt/2;

    unsigned int j = 1;
    for(unsigned int i = cnt/2+1; i<cnt+1; i++){
        x1->a[j] = x->a[i];
        j++;
    }

    dinit(x);
    return x1;
}

LongNumber* shift_left(LongNumber* X, long long int k){
    LongNumber* x = copy(X);

    unsigned int* old_a = x->a;
    x->a = realloc(x->a, (x->a[0]+k+1)*sizeof(unsigned int));

    if(!x->a){
        x->a = old_a;
        printf("Error");
        return NULL;
    }

    x->a[0] += k;

    for(unsigned int i=x->a[0]-k; i>=1; i--){
        x->a[i+k] = x->a[i];
        x->a[i] = 0;
    }

    return x;
}

LongNumber* karatsuba(LongNumber* x, LongNumber* y){
    if(x->a[0] < 4 && y->a[0] < 4){
        return multiply(x, y);
    }
    LongNumber* x0 = firstPart(x);
    LongNumber* x1 = secondPart(x);
    LongNumber* y0 = firstPart(y);
    LongNumber* y1 = secondPart(y);

    if(!x0 || !x1 || !y0 || !y1){
        if(x0) dinit(x0);
        if(x1) dinit(x1);
        if(y0) dinit(y0);
        if(y1) dinit(y1);
        printf("Error");
        return NULL;
    }

    unsigned int k = x0->a[0];

    LongNumber* p0 = karatsuba(x0, y0);
    LongNumber* p1 = karatsuba(x1, y1);
    if(!p0 || !p1){
        if(p0) dinit(p0);
        if(p1) dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    LongNumber* sum_x = copy(x0);
    LongNumber* sum_y = copy(y0);
    if(!sum_x || !sum_y){
        if(sum_x) dinit(sum_x);
        if(sum_y) dinit(sum_y);
        dinit(p0); dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    add_to_first(sum_x, x1);
    add_to_first(sum_y, y1);

    LongNumber* p2_temp = karatsuba(sum_x, sum_y);
    if(!p2_temp){
        dinit(sum_x); dinit(sum_y);
        dinit(p0); dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    LongNumber* p2 = copy(p2_temp);
    if(!p2){
        dinit(p2_temp);
        dinit(sum_x); dinit(sum_y);
        dinit(p0); dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    delete_from_first(p2, p1);
    delete_from_first(p2, p0);

    LongNumber* shifted_p1 = shift_left(p1, 2*k);
    LongNumber* shifted_p2 = shift_left(p2, k);
    if(!shifted_p1 || !shifted_p2){
        if(shifted_p1) dinit(shifted_p1);
        if(shifted_p2) dinit(shifted_p2);
        dinit(p2); dinit(p2_temp);
        dinit(sum_x); dinit(sum_y);
        dinit(p0); dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    LongNumber* result = copy(p0);
    if(!result){
        dinit(shifted_p1); dinit(shifted_p2);
        dinit(p2); dinit(p2_temp);
        dinit(sum_x); dinit(sum_y);
        dinit(p0); dinit(p1);
        dinit(x0); dinit(x1); dinit(y0); dinit(y1);
        printf("Error");
        return NULL;
    }

    add_to_first(result, shifted_p2);
    add_to_first(result, shifted_p1);

    dinit(x0); dinit(x1); dinit(y0); dinit(y1);
    dinit(p0); dinit(p1); dinit(p2); dinit(p2_temp);
    dinit(sum_x); dinit(sum_y);
    dinit(shifted_p1); dinit(shifted_p2);

    return normalize(result);
}

void print_number(LongNumber* num){
    if(num == NULL || num->a == NULL) return;

    if(num->d < 0) printf("-");

    for(unsigned int i = num->a[0]; i >= 1; i--){
        printf("%u ", num->a[i]);
    }
    printf("(len=%u, d=0x%X)\n", num->a[0], num->d);
}

int compare(LongNumber* x, LongNumber* y){
    int sign_x = (x->d >> 31) & 1;
    int sign_y = (y->d >> 31) & 1;

    if(sign_x != sign_y){
        if(sign_x == 1) return -1;
        else return 1;
    }

    if(x->a[0] != y->a[0]){
        if(sign_x == 0){
            if(x->a[0]>y->a[0]) return 1;
            else return -1;
        }
        else{
            if(x->a[0]>y->a[0]) return -1;
            else return 1;
        }
    }

    for(unsigned int i = x->a[0]; i>0; i--){
        if(sign_x == 0){
            if(x->a[i] > y->a[i]) return 1;
            else if(x->a[i] < y->a[i]) return -1;
        }
        else{
            if(x->a[i] > y->a[i]) return -1;
            else if(x->a[i] < y->a[i]) return 1;
        }
    }

    return 0;
}

LongNumber* ex3_a(LongNumber* n, LongNumber* (*multi)(LongNumber*, LongNumber*)){
    if(n->a[1]%2==0){
        return init(0);
    }
    LongNumber* i = init(1);
    LongNumber* ans = init(1);

    if(!i || !ans){
        if(i) dinit(i);
        if(ans) dinit(ans);
        return NULL;
    }

    while(compare(i, n) < 0){
        ans = multi(ans, i);
        if(!ans){
            dinit(i);
            dinit(ans);
            printf("Error");
            return NULL;
        }
        add_to_first(i, 1);
    }
    dinit(i);

    return ans;
}

LongNumber* mod_2n(unsigned long long int n, LongNumber* digit){
    unsigned long long int need = (n+7)/8;
    if(digit->a[0]>=need){
        digit->a[0] = need;

        digit->a = realloc(digit->a, (need+1)*sizeof(unsigned int));
        if(!digit->a){
            printf("Error");
            return NULL;
        }

        int sign = (digit->d >> 31) & 1;
        digit->d = digit->a[need];
        if(sign) digit->d |= 0x80000000;
    }
    return normalize(digit);
}

LongNumber* ex3_b(unsigned long long int n, LongNumber* (*multi)(LongNumber*, LongNumber*)){
    unsigned int exp = 4183;
    LongNumber* basic = init(115249);
    LongNumber* result = init(1);
    if(!basic || !result){
        if(basic) dinit(basic);
        if(result) dinit(result);
        printf("Error");
        return NULL;
    }

    while(exp>0){
        if(exp%2 != 0){
            result = multi(result, basic);
            if(!result){
                dinit(basic);
                return NULL;
            }
            result = mod_2n(n, result);
            if(!result){
                dinit(basic);
                return NULL;
            }
        }
        basic = multi(basic, basic);
        if(!basic) return NULL;
        basic = mod_2n(n, basic);
        if(!basic) return NULL;
        exp /= 2;;
    }

    dinit(basic);

    return result;
}

void time_difference(LongNumber* (*ex3)(LongNumber*, LongNumber*), unsigned long long int n ){
    LongNumber* n_long1 = init(n);
    LongNumber* n_long2 = init(n);
    if(!n_long1 || !n_long2){
        if(n_long1) dinit(n_long1);
        if(n_long2) dinit(n_long2);
        printf("Eror");
        return;
    }

    clock_t start_easy = clock();
    LongNumber* easy = ex3(n_long1, multiply);
    if(!easy){
        dinit(n_long1);
        dinit(n_long2);
        printf("Error");
        return;
    }
    clock_t end_easy = clock();

    clock_t start_karatsuba = clock();
    LongNumber* karat = ex3(n_long2, karatsuba);
    if(!karat){
        dinit(n_long1);
        dinit(n_long2);
        printf("Error");
        return;
    }
    clock_t end_karatsuba = clock();

    double time_easy = (double)(end_easy - start_easy)/CLOCKS_PER_SEC, time_karatsuba = (double)(end_karatsuba - start_karatsuba)/CLOCKS_PER_SEC;

    printf("karatsuba: %lf\tlust multiply: %lf\ntime difference: %lf\n", time_karatsuba, time_easy, (double)fabs(time_easy-time_karatsuba));

    dinit(easy);
    dinit(karat);
    dinit(n_long1);
    dinit(n_long2);
}

int main(){
    unsigned long long int n;
    scanf("%llu", &n);

    printf("3a:\n");
    time_difference(ex3_a, n);

    printf("\n3b:\n");
    time_difference(ex3_b, n);

    return 0;
}
