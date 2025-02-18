#include<stdio.h>
#include<stdlib.h>

int main(void)
{
   char str1[20];

       if (fgets(str1, sizeof(str1), stdin) == NULL) {
        fprintf(stderr, "Input error\n");
        return 1;
    }

    printf("%s\n", str1);
 return 0;
}