test : cLockQueue.o cQueue.o test.o cList.o
	gcc -Wall test.o cQueue.o cLockQueue.o cList.o -o test -pthread
cLockQueue.o : cLockQueue.c
	gcc -Wall -c cLockQueue.c -o cLockQueue.o
cQueue.o : ../cQueue/cQueue.c ../cList/cList.h
	gcc -Wall -c ../cQueue/cQueue.c -o cQueue.o
cList.o : ../cList/cList.c
	gcc -Wall -c ../cList/cList.c -o cList.o
test.o : test.c
	gcc -Wall -c test.c -o test.o
