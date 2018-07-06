function section {
    echo
    echo -e $1
    echo "----------------------------------------------------------"
}


section "REGENERATE DB"
rm diet.db
sqlite3 diet.db < diet.sql

section "BEFORE"
sqlite3 diet.db ".schema Foods"
sqlite3 diet.db "select * from Foods"

section "RUN AMPL"
ampl diet.ampl

section "AFTER"
sqlite3 diet.db ".schema Foods"
sqlite3 diet.db "select * from Foods"
