test : cStack.o test.o cList.o
	gcc -Wall test.o cStack.o cList.o -o test
cStack.o : cStack.c
	gcc -Wall -c cStack.c -o cStack.o
cList.o : ../cList/cList.c
	gcc -Wall -c ../cList/cList.c -o cList.o
test.o : test.c
	gcc -Wall -c test.c -o test.o
