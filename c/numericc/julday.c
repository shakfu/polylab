#include <math.h>
// Gregorian Calendar adopted Oct. 15, 1582.
#define IGREG (15+31L*(10+12L*1582)) 

long julday(int mm, int id, int iyyy)

/*
    In this routine julday returns the Julian Day Number that begins at noon of the calendar date
    specifed by month mm, day id, and year iyyy, all integer variables. Positive year signifes A.D.;
    negative, B.C. Remember that the year after 1 B.C. was 1 A.D.
*/

{
    void nrerror(char error_text[]);

    long jul;
    int ja,jy=iyyy,jm;
    if (jy == 0) nrerror("julday: there is no year zero.");
    if (jy < 0) ++jy;
    if (mm > 2) { // Here is an example of a block IF-structure.
        jm=mm+1;
    } else {
        --jy;
        jm=mm+13;
    }
    jul = (long) (floor(365.25*jy)+floor(30.6001*jm)+id+1720995);
    if (id+31L*(mm+12L*iyyy) >= IGREG) { // Test whether to change to Gregorian Calendar.
        ja=(int)(0.01*jy);
        jul += 2-ja+(int) (0.25*ja);
    }
    return jul;
}

