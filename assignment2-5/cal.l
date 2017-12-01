%{
#include <stdio.h>
#include <stdlib.h>
#include "cal.h"

int top_num = -1;
int top_opr = -1;
int operand[MAX_LENGTH] = {0};
int operator[MAX_LENGTH] = {0};
int neg = 0;

void process(int type);
int getResult();
void printPostfix();
%}

%%
"True"    {process(TRUE);}
"False"   {process(FALSE);}
"\\\/"    {process(CONJ);}
"\/\\"    {process(DISJ);}
"->"      {process(IMPL);}
"not"     {process(NEGT);}
"\("      {process(LEFT);}
"\)"      {process(RIGHT);}
[ \t]      ;
[\n]      {
          /*  */
          int res = getResult();
            if (res) {
              printf("The Result is True.\n");
            } else {
              printf("The Result is False.\n");
            }
          }
.       {fprintf(stderr, "Invalid character: %c\n", *yytext);}
%%


int main() {
  yylex();
  printf("Eixted.\n");
  return 0;
}

void process(int type){
  switch(type) {
    case FALSE: { // False
      top_num ++;
      if (neg) {
        operand[top_num] = TRUE;
      } else {
        operand[top_num] = FALSE;
      }
      neg = 0;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case TRUE: { // True
      top_num ++;
      if (neg) {
        operand[top_num] = FALSE;
      } else {
        operand[top_num] = TRUE;
      }
      neg = 0;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case CONJ: { // Conjontion
      top_opr ++;
      operator[top_opr] = CONJ;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case DISJ: { // Disjonction
      top_opr ++;
      operator[top_opr] = DISJ;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case IMPL: { // Implication
      top_opr ++;
      operator[top_opr] = IMPL;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case NEGT: { // Negative
      neg = 1;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case LEFT: { // Left bracket
      top_opr ++;
      operator[top_opr] = LEFT;
      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    case RIGHT: {
      // Right bracket

      while (operator[top_opr] != LEFT) {
        top_num ++;
        operand[top_num] = operator[top_opr];
        top_opr --;
      }
      top_opr --;

      //printf("top index of operand: %d\n", top_num);
      //printf("top index of operator: %d\n", top_opr);
      break;
    }
    default:;
  }
}

int getResult() {
  // printf("number of opera_num: %d\n",top_num);

  while (top_opr != -1) {
    top_num ++;
    operand[top_num] = operator[top_opr];
    top_opr --;
  }
  printPostfix();
  /**
  printf("number of opera_num: %d\n",top_num);
  int i;
  for (i = 0; i <= top_num; i++) {
    printf("%d\n",operand[i]);
  }
  */
  int index = 0;
  int write = 0;
  int res;
  while (index <= top_num) {
    switch (operand[index]) {
      case TRUE: {
        operator[write++] = operand[index++];
        break;
      }
      case FALSE: {
        operator[write++] = operand[index++];
        break;
      }
      case CONJ: {
        res = operator[write-1] && operator[write-2];
        operator[write-2] = res;
        write -= 1;
        index ++;
        break;
      }
      case DISJ: {
        res = operator[write-1] || operator[write-2];
        operator[write-2] = res;
        write -= 1;
        index ++;
        break;
      }
      case IMPL: {
        res = (1-operator[write-1]) && operator[write-2];
        operator[write-2] = res;
        write -= 1;
        index ++;
        break;
      }
      default: ;
    }
  }
  top_num = -1;
  top_opr = -1;
  return operator[0];
}

void printPostfix() {
  int i = 0;
  for (i = 0; i <= top_num; i++) {
    switch(operand[i]) {
      case 0: printf("False\n"); break;
      case 1: printf("True\n");break;
      case 2: printf("&&\n");break;
      case 3: printf("||\n");break;
      case 4: printf("->\n");break;
      case 5: printf("not\n");break;
    }
  }
}
