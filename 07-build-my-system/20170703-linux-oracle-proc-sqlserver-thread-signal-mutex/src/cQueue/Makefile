test : cQueue.o test.o cList.o
	gcc -Wall test.o cQueue.o cList.o -o test
cQueue.o : cQueue.c
	gcc -Wall -c cQueue.c -o cQueue.o
cList.o : ../cList/cList.c
	gcc -Wall -c ../cList/cList.c -o cList.o
test.o : test.c
	gcc -Wall -c test.c -o test.o
