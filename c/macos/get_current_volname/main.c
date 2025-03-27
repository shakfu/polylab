
#include <stdio.h>

#include <sys/mount.h>
#include <sys/param.h>


int main()
{
    char cpath[1024];
    struct statfs buf;

    statfs(".", &buf);
    printf("fs type name: %s\n", buf.f_fstypename);
    printf("mount name: %s\n", buf.f_mntonname);
    printf("mount from name: %s\n", buf.f_mntfromname);
    return 0;
}
