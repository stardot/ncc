/*
 * Copyright (C) Advanced Risc Machines Ltd., 1991
 * SPDX-Licence-Identifier: Apache-2.0
 */
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now = time(0);
    struct tm tms = *localtime(&now);

    fprintf(stderr, "Today is %s", ctime(&now));

/* >>>>>>>> EXPIRE at 23:59 on 01 Apr 1994 <<<<<<<< */
    tms.tm_sec  =  1;
    tms.tm_min  =  0;
    tms.tm_hour =  0;
    tms.tm_mday =  1;
    tms.tm_mon  =  9;
    tms.tm_year = 94;

    now = mktime(&tms);
    printf("#define TIME_LIMIT %d\n", now);

    fprintf(stderr, "The software expires at %s", ctime(&now));

    return 0;
}
