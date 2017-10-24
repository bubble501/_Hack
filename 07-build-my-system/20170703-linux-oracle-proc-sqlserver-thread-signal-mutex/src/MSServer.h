#ifndef MSSERVER_H
#define MSSERVER_H

#include "Entrust.h"

int MSServerInit(char *sDbUser, char *sDbPwd, char *sDbServer);
int SendEntrust(EntrustData *aEntrust, char *sTableName);

#endif
