#include <math.h>
#include <stdio.h>
#include <stdbool.h>

int bitwiseAnd(int a, int b)
{
    return a & b;
}

int bitwiseOr(int a, int b)
{
    return a | b;
}

int print_i(int i)
{
    printf("%d\n", i);
    return 0;
}

int print_f(double d)
{
    printf("%.4f\n", d);
    return 0;
}

int print_b(bool b) {
	if (b) {
		printf("true\n");
	} 
	else {
		printf("false\n");
	}
	return 0;
}