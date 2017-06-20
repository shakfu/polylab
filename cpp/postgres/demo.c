#include <stdlib.h>
#include <postgresql/libpq-fe.h>
#include <locale.h>

typedef enum {
    id,
    co,
    bu,
    company,
    name,
    nat,
    position,
    position_id,
    grade,
    reports_to,
    doj,
    dob,
    pms_type,
    score,
    pms_classification,
    rating,
    rating_1,
    rating_2,
    salary,
    salary_1,
    salary_2,
    bonus,
    bonus_1,
    bonus_2,
    loa,
    accomodation,
    house_rent,
    car_allowance,
    avg_overtime_hrs,
    avg_overtime,
    ot_allowance,
    project_allowance,
    special_allowance,
    food_allowance,
    other_allowance,
    gosi,
    leave_accrual,
    airfare_accrual,
    indemnity_accrual,
    medical_cost,
    medical_insurance,
    life_insurance,
    pa_insurance,
    school_fees,
    misc_cost,
    car_mvpi,
    car_maintenance,
    car_depreciation,
    car_insurance,
    car_fuel,
    car_registration,
    water_charges,
    electricity_charges,
    furniture_depreciation,
    house_maintenance,
    cooking_gas,
    telephone_rental,
    call_charges,
    exit_reentry_cost,
    residence_permit,
    work_permit,
    family_status,
    age,
    seniority,
    inc,
    category,
    level
} field;

char *thousands(unsigned long n)
{
	static int comma = '\0';
	static char retbuf[30];
	char *p = &retbuf[sizeof(retbuf)-1];
	int i = 0;

	if(comma == '\0') {
		struct lconv *lcp = localeconv();
		if(lcp != NULL) {
			if(lcp->thousands_sep != NULL &&
				*lcp->thousands_sep != '\0')
				comma = *lcp->thousands_sep;
			else	comma = ',';
		}
	}

	*p = '\0';

	do {
		if(i%3 == 0 && i != 0)
			*--p = comma;
		*--p = '0' + n % 10;
		n /= 10;
		i++;
	} while(n != 0);

	return p;
}

void test_pg() {
    PGconn      *conn;
    PGresult    *res;
    int         rec_count;
    int         row;
    int         col;
    double      total_salary = 0.0;
    
    conn = PQconnectdb("dbname=sa host=localhost user=sa password=sa");
    
    if (PQstatus(conn) == CONNECTION_BAD) {
        puts("We were unable to connect to the database");
        exit(0);
    }
    
    res = PQexec(conn, "select * from staff");
    
    if (PQresultStatus(res) != PGRES_TUPLES_OK) {
        puts("We did not get any data!");
        exit(0);
    }
    
    rec_count = PQntuples(res);
    printf("We received %s records.\n", thousands(rec_count));
    
    col = salary; // set column to salary
    for (row=0; row<rec_count; row++) {
        char *value = PQgetvalue(res, row, col);
        total_salary += atof(value);
    }

    printf("total salary / month: %s\n", thousands(total_salary));

    PQclear(res);

    PQfinish(conn);
}


int main()
{
    test_pg();
}

