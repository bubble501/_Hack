#ifndef ORACLE_H
#define ORACLE_H

#include <stdio.h>
#include "Entrust.h"

#define MAXCOUNT 300

int OracleInit(char *sDbUser, char *sDbPwd);
int GetEntrusts(char sMarketNo, EntrustData **Entrusts, int iCount);

#endif
