#-L、-l应该在连接时加在gcc命令上
ShEntrustTask : ShEntrustTask.o Oracle.o MSServer.o cLockQueue.o cQueue.o cList.o
	gcc -Wall ShEntrustTask.o cLockQueue.o cQueue.o cList.o Oracle.o MSServer.o -o ShEntrustTask -L /home/oracle/oracle/product/10201/lib/ -l clntsh -L /usr/local/freetds/lib -l sybdb -I /usr/local/freetds/include -pthread
#-L、-l不应该在编译时加载gcc命令上
MSServer.o : MSServer.c
	gcc -c MSServer.c -o MSServer.o
Oracle.o : Oracle.c
	gcc -Wall -c Oracle.c -o Oracle.o
ShEntrustTask.o : ShEntrustTask.c
	gcc -Wall -c ShEntrustTask.c -o ShEntrustTask.o
cLockQueue.o : ./cLockQueue/cLockQueue.c
	gcc -Wall -c ./cLockQueue/cLockQueue.c -o cLockQueue.o
cQueue.o : ./cQueue/cQueue.c
	gcc -Wall -c ./cQueue/cQueue.c -o cQueue.o
cList.o : ./cList/cList.c
	gcc -Wall -c ./cList/cList.c -o cList.o
Oracle.c : Oracle.pc
	proc Oracle.pc
