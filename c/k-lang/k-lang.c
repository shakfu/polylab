#include <math.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#define F64abs fabs
#define F64sqrt sqrt
#define F64floor floor
#define F64min fmin
#define F64max fmax
#define F64copysign copysign
#define Exit exit
static char *M_, *M2_;
static int32_t* I_;
static uint64_t *U_, *U2_;
static void** F_;
static int32_t memorysize_, memorysize2_;
static int args_;
static char** argv_;
static jmp_buf jb_;
static int jb__ = 0;

static void Memory(int32_t x)
{
    memorysize_ = x;
    U_ = (uint64_t*)calloc((size_t)x, 64 * 1024);
    M_ = (char*)U_;
    I_ = (int32_t*)U_;
}

static void Memory2(int32_t x)
{
    memorysize2_ = x;
    U2_ = (uint64_t*)calloc((size_t)x, 64 * 1024);
    M2_ = (char*)U2_;
}

static int32_t Memorysize(void) { return memorysize_; }

static int32_t Memorysize2(void) { return memorysize2_; }

static int32_t Memorygrow(int32_t delta)
{
    int32_t r = memorysize_;
    memorysize_ += delta;
    U_ = (uint64_t*)realloc(U_, 64 * 1024 * (size_t)memorysize_);
    M_ = (char*)U_;
    I_ = (int32_t*)U_;
    return r;
}

static int32_t Memorygrow2(int32_t delta)
{
    int32_t r = memorysize2_;
    memorysize2_ += delta;
    U2_ = (uint64_t*)realloc(U2_, 64 * 1024 * (size_t)memorysize2_);
    M2_ = (char*)U2_;
    return r;
}

#define I8(x) (int8_t)(M_[x])
#define I32(x) (I_[(x) >> 2])
#define I64(x) (int64_t)(U_[(x) >> 3])
#define F64(x) ((double*)U_)[(x) >> 3]

static void SetI8(int32_t x, int32_t y) { M_[x] = (char)(y); }

static void SetI32(int32_t x, int32_t y) { I_[(x) >> 2] = (y); }

static void SetI64(int32_t x, int64_t y) { U_[(x) >> 3] = (uint64_t)(y); }

static void SetF64(int32_t x, double y) { ((double*)U_)[(x) >> 3] = (y); }

#define I32B(x) (int32_t)(x)

static void Memorycopy(int32_t dst, int32_t src, int32_t n)
{
    memcpy(M_ + dst, M_ + src, (size_t)n);
}

static void Memorycopy2(int32_t dst, int32_t src, int32_t n)
{
    memcpy(M2_ + dst, M_ + src, (size_t)n);
}

static void Memorycopy3(int32_t dst, int32_t src, int32_t n)
{
    memcpy(M_ + dst, M2_ + src, (size_t)n);
}

static void Memoryfill(int32_t p, int32_t v, int32_t n)
{
    memset(M_ + p, (int)v, (size_t)n);
}

static int32_t I32clz(int32_t x)
{
    return (int32_t)__builtin_clz((unsigned int)x);
}

static double F64reinterpret_i64(uint64_t x)
{
    union {
        uint64_t i;
        double f;
    } u;

    u.i = x;
    return u.f;
}

static uint64_t I64reinterpret_f64(double x)
{
    union {
        uint64_t i;
        double f;
    } u;

    u.f = x;
    return u.i;
}

static int32_t Args(void) { return args_; }

static int32_t Arg(int32_t i, int32_t r)
{
    if (i >= args_)
        return 0;
    if (r == 0)
        return (int32_t)strlen(argv_[i]);
    memcpy(M_ + r, argv_[i], strlen(argv_[i]));
    return 0;
}

static int32_t Read(int32_t file, int32_t nfile, int32_t dst)
{
    static char* filebuf = NULL;
    static size_t n = 0;
    if (dst != 0) {
        memcpy(M_ + dst, filebuf, n);
        return 0;
    }
    char name[512];
    if (nfile > 511)
        return -1;
    memcpy(name, M_ + file, (size_t)nfile);
    name[nfile] = (char)0;
    FILE* fp = fopen(name, "rb");
    if (fp == NULL) {
        if (filebuf != NULL)
            free(filebuf);
        n = 0;
        return -1;
    }
    fseek(fp, 0, SEEK_END);
    n = (size_t)ftell(fp);
    fseek(fp, 0, SEEK_SET);
    if (filebuf != NULL)
        free(filebuf);
    filebuf = malloc(n);
    if (n != fread(filebuf, 1, n, fp)) {
        fclose(fp);
        return -1;
    }
    fclose(fp);
    return (int32_t)n;
}

static int32_t Write(int32_t file, int32_t nfile, int32_t src, int32_t n)
{
    if (nfile == 0) {
        fwrite(M_ + src, 1, (size_t)n, stdout);
        return 0;
    }
    char name[512];
    memcpy(name, M_ + file, (size_t)nfile);
    name[nfile] = (char)0;
    FILE* fp = fopen(name, "wb");
    if (fp == NULL) {
        return -1;
    }
    fwrite(M_ + src, 1, (size_t)n, fp);
    fclose(fp);
    return 0;
}

static int32_t ReadIn(int32_t dst, int32_t n)
{
    char* line = readline(" ");
    if (line == NULL) { // eof
        return 0;
    }

    size_t len = strlen(line);
    if (len > 0 && line[len - 1] != '\n') {
        // readline doesn't include newline, add it for compatibility
        if (len + 1 < (size_t)n) {
            memcpy(M_ + dst, line, len);
            M_[dst + len] = '\n';
            len++;
        } else {
            memcpy(M_ + dst, line, (size_t)(n - 1));
            len = (size_t)(n - 1);
        }
    } else {
        size_t copy_len = (len < (size_t)n) ? len : (size_t)(n - 1);
        memcpy(M_ + dst, line, copy_len);
        len = copy_len;
    }

    // Add to history if non-empty and not just whitespace
    if (len > 0) {
        int has_content = 0;
        for (size_t i = 0; i < len; i++) {
            if (line[i] != ' ' && line[i] != '\t' && line[i] != '\n') {
                has_content = 1;
                break;
            }
        }
        if (has_content) {
            add_history(line);
        }
    }

    free(line);
    return (int32_t)len;
}

static int64_t Native(int64_t x, int64_t y)
{
#ifdef NATIVE
    return cnative(x, y);
#else
    return 0 * (x + y);
#endif
}

static void panic(int32_t x)
{
    if (!jb__)
        exit(1);
    longjmp(jb_, 1);
}

uint64_t src(void);
static void write(uint64_t);
static uint64_t Ku(uint64_t);
static int32_t maxi(int32_t, int32_t);
static int32_t mini(int32_t, int32_t);
int32_t nn(uint64_t);
static uint64_t Cat(uint64_t, uint64_t);
uint64_t Kc(int32_t);
static uint64_t ntake(int32_t, uint64_t);
static uint64_t ti(int32_t, int32_t);
uint64_t l2(uint64_t, uint64_t);
int32_t tp(uint64_t);
static int32_t isfunc(int32_t);
static uint64_t Bin(uint64_t, uint64_t);
static uint64_t Fst(uint64_t);
static uint64_t ecn(uint64_t, uint64_t);
static uint64_t x0(uint64_t);
static uint64_t Key(uint64_t, uint64_t);
static uint64_t l1(uint64_t);
static uint64_t r1(uint64_t);
static uint64_t explode(uint64_t);
static uint64_t prior(int32_t, uint64_t);
uint64_t mk(int32_t, int32_t);
uint64_t Atx(uint64_t, uint64_t);
uint64_t rx(uint64_t);
static uint64_t ati(uint64_t, int32_t);
static void dxy(uint64_t, uint64_t);
static uint64_t uf(uint64_t);
static uint64_t key(uint64_t, uint64_t, int32_t);
static uint64_t ec2(uint64_t, uint64_t, uint64_t);
static uint64_t Flp(uint64_t);
static int32_t dtypes(uint64_t, uint64_t);
static uint64_t dkeys(uint64_t, uint64_t);
static uint64_t dvals(uint64_t);
static int32_t conform(uint64_t, uint64_t);
uint64_t Cal(uint64_t, uint64_t);
void dx(uint64_t);
static uint64_t join(uint64_t, uint64_t);
static uint64_t Dec(uint64_t, uint64_t);
static int32_t arity(uint64_t);
static uint64_t rdn(uint64_t, uint64_t, uint64_t);
static uint64_t fix(uint64_t, uint64_t, uint64_t);
static uint64_t Ecr(uint64_t, uint64_t);
uint64_t Val(uint64_t);
static int32_t ep(uint64_t);
static uint64_t missing(int32_t);
static uint64_t cal(uint64_t, uint64_t);
static uint64_t ndrop(int32_t, uint64_t);
static uint64_t cat1(uint64_t, uint64_t);
static uint64_t x1(uint64_t);
static uint64_t r0(uint64_t);
static int32_t match(uint64_t, uint64_t);
static uint64_t split(uint64_t, uint64_t);
static uint64_t Enc(uint64_t, uint64_t);
static uint64_t Enl(uint64_t);
static uint64_t Ecl(uint64_t, uint64_t);
static uint64_t Til(uint64_t);
static uint64_t sti(uint64_t, int32_t, uint64_t);
uint64_t Ki(int32_t);
static uint64_t atv(uint64_t, uint64_t);
static uint64_t Add(uint64_t, uint64_t);
static uint64_t seq(int32_t);
static int32_t bucket(int32_t);
static int32_t grow(int32_t);
static int32_t sz(int32_t);
static uint64_t atdepth(uint64_t, uint64_t);
static uint64_t x2(uint64_t);
static uint64_t calltrain(uint64_t, uint64_t);
static uint64_t callprj(uint64_t, uint64_t);
static uint64_t lambda(uint64_t, uint64_t);
static uint64_t native(uint64_t, uint64_t);
static uint64_t prj(uint64_t, uint64_t);
static uint64_t stv(uint64_t, uint64_t, uint64_t);
static uint64_t exec(uint64_t);
static uint64_t Drp(uint64_t, uint64_t);
static uint64_t l3(uint64_t, uint64_t, uint64_t);
static uint64_t dcat(uint64_t, uint64_t);
static uint64_t ucat(uint64_t, uint64_t);
static uint64_t uspc(uint64_t, int32_t, int32_t);
static void push(uint64_t);
static int32_t marksrc(uint64_t);
static uint64_t pop(void);
static int32_t h48(uint64_t);
static uint64_t Dmd(uint64_t, uint64_t, uint64_t, uint64_t);
static uint64_t Unq(uint64_t);
static uint64_t Fnd(uint64_t, uint64_t);
static uint64_t use(uint64_t);
static uint64_t Grp(uint64_t);
static uint64_t deal(uint64_t, uint64_t);
static int32_t fnd(uint64_t, uint64_t, int32_t);
static uint64_t fdl(uint64_t, uint64_t);
static int32_t findat(int32_t, int32_t, int32_t);
static int32_t cmF(int32_t, int32_t);
static int32_t cmZ(int32_t, int32_t);
static int32_t ts(uint64_t);
static uint64_t rtp(int32_t, uint64_t);
uint64_t sc(uint64_t);
uint64_t cs(uint64_t);
static uint64_t Rot(uint64_t, uint64_t);
static uint64_t uptype(uint64_t, int32_t);
static uint64_t Kz(double, double);
uint64_t Kf(double);
static uint64_t Wer(uint64_t);
static uint64_t Min(uint64_t, uint64_t);
static uint64_t Mor(uint64_t, uint64_t);
static uint64_t Les(uint64_t, uint64_t);
static void zk(void);
static void doargs(void);
static void store(void);
static uint64_t readfile(uint64_t);
static void try(uint64_t);
void repl(uint64_t);
static uint64_t getargv(void);
static void dofile(uint64_t, uint64_t);
static uint64_t val(uint64_t);
static void test(uint64_t);
static void cosin_(double, int32_t, int32_t);
static double atan2_(double, double);
static double exp_(double);
static double log_(double);
static double pow_(double, double);
static double atan_(double);
static double satan(double);
static double xatan(double);
static double expmulti(double, double, int64_t);
static double ldexp_(double, int64_t);
static double normalize(double);
static int32_t iipow(int32_t, int32_t);
static uint64_t parse(uint64_t);
static uint64_t Tok(uint64_t);
static uint64_t es(void);
static uint64_t next(void);
static uint64_t e(uint64_t);
static uint64_t t(void);
static uint64_t pasn(uint64_t, uint64_t, uint64_t);
static uint64_t dyadic(uint64_t, uint64_t);
static uint64_t idiom(uint64_t);
static uint64_t monadic(uint64_t);
static int32_t is(int32_t, int32_t);
static uint64_t rlist(uint64_t, uint64_t);
static uint64_t plist(uint64_t);
static uint64_t plam(int32_t);
static uint64_t pspec(uint64_t, uint64_t);
static uint64_t lastp(uint64_t);
static uint64_t ldrop(int32_t, uint64_t);
static uint64_t cond(uint64_t, int32_t);
static uint64_t whl(uint64_t, int32_t);
static uint64_t clist(uint64_t);
static uint64_t Rev(uint64_t);
static uint64_t Flr(uint64_t);
static uint64_t randI(int32_t, int32_t);
static uint64_t shuffle(uint64_t, int32_t);
static int32_t minis(int32_t, int32_t, int32_t);
static double minfs(int32_t, int32_t);
static int32_t maxis(int32_t, int32_t, int32_t);
static double maxfs(int32_t, int32_t);
static int32_t sumi(int32_t, int32_t);
static double sumf(int32_t, int32_t);
static void sumz(int32_t, int32_t, int32_t);
static uint64_t nm(int32_t, uint64_t);
static uint64_t absZ(uint64_t);
static uint64_t reim(uint64_t, int32_t);
static uint64_t Mul(uint64_t, uint64_t);
static uint64_t nd(int32_t, int32_t, uint64_t, uint64_t);
static uint64_t nc(int32_t, int32_t, uint64_t, uint64_t);
static uint64_t writefile(uint64_t, uint64_t);
static uint64_t nf(int32_t, uint64_t, uint64_t);
static double fk(uint64_t);
static uint64_t Asc(uint64_t);
static uint64_t mat(uint64_t, uint64_t);
static uint64_t grade(uint64_t, int32_t);
static void msrt(int32_t, int32_t, int32_t, int32_t, int32_t, int32_t,
                 int32_t);
static void mrge(int32_t, int32_t, int32_t, int32_t, int32_t, int32_t, int32_t,
                 int32_t);
static uint64_t emb(int32_t, int32_t, uint64_t);
static uint64_t si(int32_t);
static uint64_t sf(double);
static uint64_t sfz(double, double);
static uint64_t se(double);
static uint64_t trdot(uint64_t);
static uint64_t Out(uint64_t);
static void testi(uint64_t, int32_t);
static void ws(void);
static uint64_t thex(void);
static int32_t cq(int32_t);
static int32_t hx(int32_t);
static uint64_t tnum(void);
static uint64_t tunm(void);
static int64_t pu(void);
static uint64_t pflt(int64_t);
static uint64_t ppi(double);
static uint64_t pflz(double);
static double pexp(double);
static int32_t maxcount(int32_t, int32_t);
static uint64_t Tak(uint64_t, uint64_t);
static uint64_t cuts(uint64_t, uint64_t);
static uint64_t rcut(uint64_t, uint64_t, uint64_t);
static int32_t ibin(uint64_t, uint64_t, int32_t);
static int32_t lc(int32_t);
static uint64_t lower(uint64_t);
static const int32_t ct = 2;
static const int32_t it = 3;
static const int32_t st = 4;
static const int32_t ft = 5;
static const int32_t zt = 6;
static const int32_t cf = 10;
static const int32_t df = 11;
static const int32_t pf = 12;
static const int32_t lf = 13;
static const int32_t xf = 14;
static const int32_t Ct = 18;
static const int32_t It = 19;
static const int32_t St = 20;
static const int32_t Ft = 21;
static const int32_t Zt = 22;
static const int32_t Lt = 23;
static const int32_t Dt = 24;
static const int32_t Tt = 25;
static const double pi = 3.141592653589793;
static const double maxfloat = 1.7976931348623157e+308;
static const int32_t b0 = 5;
static const int32_t bs = 15;
static const int32_t vl = 16;
static uint64_t loc = 0ull;
static uint64_t xyz = 0ull;
static double na = 0.;
static double inf = 0.;
static int32_t pp = 0;
static int32_t pe = 0;
static int32_t sp = 0;
static int32_t srcp = 0;
static int32_t rand_ = 0;
static int32_t ps = 0;

void trap(void)
{
    uint64_t s;
    int32_t a3, b3, i4;
    s = src();
    if (!srcp) {
        write(Ku(2608ull));
    } else {
        a3 = maxi((srcp - 30), 0);
        b3 = mini(nn(s), (srcp + 30));
        {
            i4 = a3;
            for (; (i4 < b3); i4++)
                if (I8(((int32_t)(s) + i4)) == 10) {
                    if (i4 < srcp) {
                        a3 = (1 + i4);
                    } else {
                        b3 = i4;
                    }
                }
        }
        Write(0, 0, ((int32_t)(s) + a3), (b3 - a3));
        if (srcp > a3) {
            write(Cat(Kc(10), ntake(((srcp - a3) - 1), Kc(32))));
        }
    }
    write(Ku(2654ull));
    panic(srcp);
}

static uint64_t ech(uint64_t x) { return ti(df, (int32_t)(l2(x, 0ull))); }

static uint64_t rdc(uint64_t x) { return ti(df, (int32_t)(l2(x, 2ull))); }

static uint64_t scn(uint64_t x) { return ti(df, (int32_t)(l2(x, 4ull))); }

static uint64_t Ech(uint64_t f, uint64_t x)
{
    uint64_t r;
    int32_t t, xt, xn, rp, i14;
    t = tp(f);
    if (!isfunc(t)) {
        return Bin(f, Fst(x));
    }
    if (nn(x) == 1) {
        x = Fst(x);
    } else {
        return ecn(f, x);
    }
    if (tp(x) < 16) {
        trap();
    }
    xt = tp(x);
    if (xt == Dt) {
        r = x0(x);
        return Key(r, Ech(f, l1(r1(x))));
    }
    if (xt == Tt) {
        x = explode(x);
    }
    if (((t == 0) && (xt < Lt)) && ((int32_t)(f) < 11)) {
        return prior((int32_t)(f), x);
    }
    xn = nn(x);
    r = mk(Lt, xn);
    rp = (int32_t)(r);
    {
        i14 = (int32_t)(0);
        for (; (i14 < xn); i14++) {
            SetI64(rp, (int64_t)(Atx(rx(f), ati(rx(x), i14))));
            rp += 8;
        }
    }
    dxy(f, x);
    return uf(r);
}

static uint64_t ecn(uint64_t f, uint64_t x)
{
    uint64_t r2;
    if (nn(x) == 2) {
        r2 = x0(x);
        x = r1(x);
        if ((tp(f) == 0) && ((int32_t)(f) == 13)) {
            if ((tp(r2) == Tt) && (tp(x) == Tt)) {
                if (nn(r2) != nn(x)) {
                    trap();
                }
                f = Cat(x0(r2), x0(x));
                return key(f, Cat(r1(r2), r1(x)), Tt);
            }
        }
        return ec2(f, r2, x);
    }
    return Ech(20ull, l2(f, Flp(x)));
}

static uint64_t ec2(uint64_t f, uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t t, n, rp, i7;
    t = dtypes(x, y);
    if (t > Lt) {
        r = dkeys(x, y);
        return key(r, ec2(f, dvals(x), dvals(y)), t);
    }
    n = conform(x, y);
    switch (n) {
    case 0:
        return Cal(f, l2(x, y));
        break;
    case 1:
        n = nn(y);
        break;
    default:
        n = nn(x);
        break;
    }
    r = mk(Lt, n);
    rp = (int32_t)(r);
    {
        i7 = (int32_t)(0);
        for (; (i7 < n); i7++) {
            SetI64(rp,
                   (int64_t)(Cal(rx(f), l2(ati(rx(x), i7), ati(rx(y), i7)))));
            rp += 8;
        }
    }
    dx(f);
    dxy(x, y);
    return uf(r);
}

static uint64_t Rdc(uint64_t f, uint64_t x)
{
    uint64_t r22, x0;
    int32_t t, a, xt, xn, fp20, i;
    t = tp(f);
    if (!isfunc(t)) {
        if (nn(x) == 2) {
            trap();
        }
        x = Fst(x);
        return ((t & 15) == ct) ? (join(f, x)) : (Dec(f, x));
    }
    a = arity(f);
    if (a != 2) {
        return (a > 2) ? (rdn(f, x, 0ull)) : (fix(f, Fst(x), 0ull));
    }
    if (nn(x) == 2) {
        return Ecr(f, x);
    }
    x = Fst(x);
    xt = tp(x);
    if (xt == Dt) {
        x = Val(x);
        xt = tp(x);
    }
    if (xt < 16) {
        dx(f);
        return x;
    }
    xn = nn(x);
    if (!t) {
        fp20 = (int32_t)(f);
        if ((fp20 > 1) && (fp20 < 8)) {
            if (xt == Tt) {
                return Ech(rdc(f), l1(Flp(x)));
            }
            r22 = ((uint64_t (*)(int32_t, int32_t, int32_t))F_[(256 + fp20)])(
                (int32_t)(x), xt, ep(x));
            if (r22 != 0ull) {
                dx(x);
                return r22;
            }
        }
        if (fp20 == 13) {
            if (xt < Lt) {
                return x;
            }
        }
    }
    if (!xn) {
        dxy(f, x);
        return missing(xt);
    }
    i = (int32_t)(1);
    x0 = ati(rx(x), 0);
    for (; (i < xn);) {
        x0 = cal(rx(f), l2(x0, ati(rx(x), i)));
        i++;
    }
    dxy(x, f);
    return x0;
}

static uint64_t rdn(uint64_t f, uint64_t x, uint64_t l)
{
    uint64_t r;
    int32_t n, i1;
    r = Fst(rx(x));
    x = Flp(ndrop(1, x));
    n = nn(x);
    {
        i1 = (int32_t)(0);
        for (; (i1 < n); i1++) {
            r = Cal(rx(f), Cat(l1(r), ati(rx(x), i1)));
            if (l != 0ull) {
                l = cat1(l, rx(r));
            }
        }
    }
    dxy(f, x);
    if (l != 0ull) {
        dx(r);
        return uf(l);
    }
    return r;
}

static uint64_t Ecr(uint64_t f, uint64_t x)
{
    uint64_t r, y;
    int32_t yt, t4, yn, rp, i5;
    y = x1(x);
    x = r0(x);
    yt = tp(y);
    if (yt < 16) {
        return cal(f, l2(x, y));
    }
    if (yt > Lt) {
        t4 = dtypes(x, y);
        r = dkeys(x, y);
        return key(r, Ecr(f, l2(dvals(x), dvals(y))), t4);
    }
    yn = nn(y);
    r = mk(Lt, yn);
    rp = (int32_t)(r);
    {
        i5 = (int32_t)(0);
        for (; (i5 < yn); i5++) {
            SetI64(rp, (int64_t)(cal(rx(f), l2(rx(x), ati(rx(y), i5)))));
            rp += 8;
        }
    }
    dx(f);
    dxy(x, y);
    return uf(r);
}

static uint64_t fix(uint64_t f, uint64_t x, uint64_t l)
{
    uint64_t r, y;
    r = (uint64_t)(0ull);
    y = rx(x);
    for (;;) {
        r = Atx(rx(f), rx(x));
        if (match(r, x)) {
            break;
        }
        if (match(r, y)) {
            break;
        }
        dx(x);
        x = r;
        if (l != 0ull) {
            l = cat1(l, rx(x));
        }
    }
    dx(f);
    dxy(r, y);
    if (l != 0ull) {
        dx(x);
        return l;
    }
    return x;
}

static uint64_t Scn(uint64_t f, uint64_t x)
{
    uint64_t r, z;
    int32_t t, a, xt, xn, rp, i;
    t = tp(f);
    if (!isfunc(t)) {
        if (nn(x) != 1) {
            trap();
        }
        x = Fst(x);
        return ((t & 15) == ct) ? (split(f, x)) : (Enc(f, x));
    }
    a = arity(f);
    if (a != 2) {
        return (a > 2) ? (rdn(f, x, mk(Lt, 0))) : ({
            x = rx(Fst(x));
            fix(f, x, Enl(x));
        });
    }
    if (nn(x) == 2) {
        return Ecl(f, x);
    }
    x = Fst(x);
    xt = tp(x);
    if (xt < 16) {
        dx(f);
        return x;
    }
    xn = nn(x);
    if (!xn) {
        dx(f);
        return x;
    }
    if (xt == Dt) {
        r = x0(x);
        return Key(r, Scn(f, l1(r1(x))));
    }
    r = mk(Lt, xn);
    rp = (int32_t)(r);
    i = (int32_t)(1);
    z = ati(rx(x), 0);
    SetI64(rp, (int64_t)(rx(z)));
    rp += 8;
    for (; (i < xn);) {
        z = cal(rx(f), l2(z, ati(rx(x), i)));
        SetI64(rp, (int64_t)(rx(z)));
        rp += 8;
        i++;
    }
    dx(z);
    dxy(x, f);
    return uf(r);
}

static uint64_t Ecl(uint64_t f, uint64_t x)
{
    uint64_t y, r;
    int32_t xn, rp, i3;
    y = x1(x);
    x = r0(x);
    if (tp(x) < 16) {
        return cal(f, l2(x, y));
    }
    xn = nn(x);
    r = mk(Lt, xn);
    rp = (int32_t)(r);
    {
        i3 = (int32_t)(0);
        for (; (i3 < xn); i3++) {
            SetI64(rp, (int64_t)(cal(rx(f), l2(ati(rx(x), i3), rx(y)))));
            rp += 8;
        }
    }
    dx(f);
    dxy(x, y);
    return uf(r);
}

static uint64_t uf(uint64_t x)
{
    uint64_t r8, r;
    int32_t rt, xn, xp, i1, t2;
    rt = (int32_t)(0);
    xn = nn(x);
    xp = (int32_t)(x);
    {
        i1 = (int32_t)(0);
        for (; (i1 < xn); i1++) {
            t2 = tp((uint64_t)(I64(xp)));
            if (!i1) {
                rt = t2;
            } else {
                if (t2 != rt) {
                    return x;
                }
            }
            xp += 8;
        }
    }
    if (rt == Dt) {
        r8 = Til(x0(x));
        if (tp(r8) != St) {
            dx(r8);
            return x;
        }
        xp = (int32_t)(x);
        for (; (xn > 0);) {
            xn = (xn - 1);
            if (!match(r8, (uint64_t)(I64((int32_t)(I64(xp)))))) {
                dx(r8);
                return x;
            }
            xp += 8;
        }
        return key(r8, Flp(Ech(20ull, l1(x))), Tt);
    }
    if ((rt == 0) || (rt > zt)) {
        return x;
    }
    r = mk((rt + 16), xn);
    for (; (xn > 0);) {
        xn = (xn - 1);
        r = sti(r, xn, ati(rx(x), xn));
    }
    dx(x);
    return r;
}

static uint64_t prior(int32_t f, uint64_t x)
{
    uint64_t a;
    int32_t n;
    n = nn(x);
    if (!n) {
        if (f < 8) {
            dx(x);
            return mk(It, 0);
        }
        return x;
    }
    a = Ki(0);
    if (f < 8) {
        a = ati(rx(x), ((n - 1) * I32B((f == 1))));
    }
    return sti(((uint64_t (*)(uint64_t, uint64_t))F_[(f + 64)])(
                   x, atv(rx(x), Add(Ki(-1), seq(n)))),
               0, a);
}

static void minit(int32_t a, int32_t b)
{
    int32_t p;
    p = (int32_t)((1 << a));
    for (; (a < b);) {
        SetI32((4 * a), p);
        SetI32(p, 0);
        p = (p * 2);
        a++;
    }
    SetI32(128, b);
}

static int32_t alloc(int32_t n, int32_t s)
{
    int32_t size, t, i, m, a, j8, u9;
    size = (n * s);
    t = bucket(size);
    if (((int64_t)(n) * (int64_t)(s)) > 2147483647ll) {
        trap();
    }
    i = (4 * t);
    m = (4 * I32(128));
    for (; (!I32(i));)
        if (i >= m) {
            m = (4 * grow(i));
        } else {
            i += 4;
        }
    a = I32(i);
    SetI32(i, I32(a));
    {
        j8 = (i - 4);
        for (; (j8 >= (4 * t)); j8 = (j8 - 4)) {
            u9 = (a + ((int32_t)(1) << (j8 >> 2)));
            SetI32(u9, I32(j8));
            SetI32(j8, u9);
        }
    }
    if ((a & 31)) {
        trap();
    }
    return (a + vl);
}

static int32_t grow(int32_t p)
{
    int32_t n, g;
    n = (1 + (p >> 2));
    g = ((1 << (n - 16)) - Memorysize());
    if (g > 0) {
        if (Memorygrow(g) < 0) {
            trap();
        }
    }
    minit(I32(128), n);
    return n;
}

static void mfree(int32_t x, int32_t bs)
{
    int32_t t;
    if ((x & 31)) {
        trap();
    }
    t = (4 * bs);
    SetI32(x, I32(t));
    SetI32(t, x);
}

static int32_t bucket(int32_t size)
{
    return maxi(b0, ((int32_t)(32) - I32clz((bs + size))));
}

uint64_t mk(int32_t t, int32_t n)
{
    int32_t x;
    if (t < 17) {
        trap();
    }
    x = alloc(n, sz(t));
    SetI32((x - 4), 1);
    SetI32((x - 12), n);
    return ti(t, x);
}

int32_t tp(uint64_t x) { return (int32_t)(((uint64_t)(x) >> 59ull)); }

int32_t nn(uint64_t x) { return I32(((int32_t)(x)-12)); }

static int32_t ep(uint64_t x) { return ((int32_t)(x) + (sz(tp(x)) * nn(x))); }

static int32_t sz(int32_t t)
{
    if (t < 16) {
        return 8;
    } else {
        if (t < 19) {
            return 1;
        } else {
            if (t < 21) {
                return 4;
            } else {
                if (t == Zt) {
                    return 16;
                }
            }
        }
    }
    return 8;
}

uint64_t rx(uint64_t x)
{
    int32_t p2;
    if (tp(x) > 4) {
        p2 = ((int32_t)(x)-4);
        SetI32(p2, (1 + I32(p2)));
    }
    return x;
}

void dx(uint64_t x)
{
    int32_t t, p, rc, n6, p8, e8;
    t = tp(x);
    if (t < 5) {
        return;
    }
    p = ((int32_t)(x)-vl);
    rc = I32(((p + vl) - 4));
    SetI32(((p + vl) - 4), (rc - 1));
    if (!rc) {
        trap();
    }
    if (rc == 1) {
        n6 = nn(x);
        if ((t & 15) > 6) {
            if (((t == 14) || (t == 24)) || (t == 25)) {
                n6 = 2;
            } else {
                if ((t == 12) || (t == 13)) {
                    n6 = 3;
                }
            }
            p8 = (int32_t)(x);
            e8 = (p8 + (8 * n6));
            for (; (p8 < e8);) {
                dx((uint64_t)(I64(p8)));
                p8 += 8;
            }
        }
        mfree(p, bucket((sz(t) * n6)));
    }
}

static void dxy(uint64_t x, uint64_t y)
{
    dx(x);
    dx(y);
}

static void rl(uint64_t x)
{
    int32_t e, p;
    e = ep(x);
    p = (int32_t)(x);
    for (; (e > p);) {
        e = (e - 8);
        rx((uint64_t)(I64(e)));
    }
}

uint64_t Cal(uint64_t x, uint64_t y)
{
    y = explode(y);
    if (isfunc(tp(x))) {
        return cal(x, y);
    }
    return atdepth(x, y);
}

static int32_t isfunc(int32_t t)
{
    return I32B(((t == 0) || ((uint32_t)((t - 10)) < 5)));
}

static uint64_t cal(uint64_t f, uint64_t x)
{
    uint64_t r, z, y, d24;
    int32_t t, fp, xn, a24;
    r = (uint64_t)(0ull);
    z = (uint64_t)(0ull);
    y = (uint64_t)(0ull);
    t = tp(f);
    fp = (int32_t)(f);
    xn = nn(x);
    if (t < df) {
        switch ((xn - 1)) {
        case 0:
            x = Fst(x);
            break;
        case 1: {
            r = x1(x);
            x = r0(x);
        } break;
        default: {
            r = x1(x);
            z = x2(x);
            if (xn == 4) {
                y = x0((x + 24ull));
            }
            x = r0(x);
        } break;
        }
    }
    if (t) {
        t = (t - 9);
    }
    switch (t) {
    case 0: {
        switch ((xn - 1)) {
        case 0:
            r = ((uint64_t (*)(uint64_t))F_[(int32_t)(f)])(x);
            break;
        case 1:
            r = ((uint64_t (*)(uint64_t, uint64_t))F_[(fp + 64)])(x, r);
            break;
        case 2:
            r = ((uint64_t (*)(uint64_t, uint64_t, uint64_t,
                               uint64_t))F_[(fp + 192)])(x, r, 1ull, z);
            break;
        case 3:
            r = ((uint64_t (*)(uint64_t, uint64_t, uint64_t,
                               uint64_t))F_[(fp + 192)])(x, r, z, y);
            break;
        default: {
            trap();
            r = 0ull;
        } break;
        }
    } break;
    case 1: {
        switch ((xn - 1)) {
        case 0:
            r = calltrain(f, l1(x));
            break;
        case 1:
            r = calltrain(f, l2(x, r));
            break;
        default: {
            trap();
            r = 0ull;
        } break;
        }
    } break;
    case 2: {
        d24 = x0(f);
        a24 = (85 + (int32_t)(I64((fp + 8))));
        r = ((uint64_t (*)(uint64_t, uint64_t))F_[a24])(d24, x);
    } break;
    case 3:
        r = callprj(f, x);
        break;
    case 4:
        r = lambda(f, x);
        break;
    case 5:
        r = native(f, x);
        break;
    default: {
        trap();
        r = 0ull;
    } break;
    }
    dx(f);
    return r;
}

static uint64_t calltrain(uint64_t f, uint64_t x)
{
    return cal(x0((f + 8ull)), l1(cal(x0(f), x)));
}

static uint64_t callprj(uint64_t f, uint64_t x)
{
    int32_t n, fn;
    n = nn(x);
    fn = nn(f);
    if (fn != n) {
        if (n < fn) {
            rx(f);
            return prj(f, x);
        }
        trap();
    }
    return Cal(x0(f), stv(x1(f), x2(f), x));
}

static uint64_t native(uint64_t f, uint64_t x)
{
    int32_t fn, xn;
    fn = nn(f);
    xn = nn(x);
    if (xn != fn) {
        if (xn < fn) {
            rx(f);
            return prj(f, x);
        }
        trap();
    }
    return (uint64_t)(Native((int64_t)(x0(f)), (int64_t)(x)));
}

static uint64_t lambda(uint64_t f, uint64_t x)
{
    uint64_t lo, z;
    int32_t fn, xn, n, a, zp, xp, vp, p6, e, p11;
    fn = nn(f);
    xn = nn(x);
    if (xn < fn) {
        rx(f);
        return prj(f, x);
    }
    if (xn != fn) {
        trap();
    }
    lo = (uint64_t)(I64(((int32_t)(f) + 16)));
    n = nn(lo);
    a = nn(f);
    z = mk(Zt, n);
    zp = (int32_t)(z);
    xp = ep(x);
    vp = I32(8);
    for (; (n > 0);) {
        n = (n - 1);
        p6 = I32(((int32_t)(lo) + (4 * n)));
        SetI32(zp, p6);
        p6 += vp;
        SetI64((zp + 8), I64(p6));
        if (n < a) {
            xp = (xp - 8);
            SetI64(p6, I64(xp));
        } else {
            SetI64(p6, 0ll);
        }
        zp += 16;
    }
    rl(x);
    dx(x);
    x = exec(x0(f));
    zp = (int32_t)(z);
    e = ep(z);
    for (; (zp < e);) {
        p11 = (I32(8) + I32(zp));
        dx((uint64_t)(I64(p11)));
        SetI64(p11, I64((zp + 8)));
        zp += 16;
    }
    dx(z);
    return x;
}

static uint64_t com(uint64_t x, uint64_t y)
{
    return ti(cf, (int32_t)(l2(y, x)));
}

static uint64_t prj(uint64_t f, uint64_t x)
{
    uint64_t r, a, y10;
    int32_t xn, xp, i3, ar, i7, an;
    if (!isfunc(tp(f))) {
        return atdepth(f, x);
    }
    xn = nn(x);
    xp = (int32_t)(x);
    a = mk(It, 0);
    {
        i3 = (int32_t)(0);
        for (; (i3 < xn); i3++) {
            if (I64(xp) == 0ll) {
                a = cat1(a, Ki(i3));
            }
            xp += 8;
        }
    }
    ar = arity(f);
    {
        i7 = xn;
        for (; (i7 < ar); i7++) {
            a = cat1(a, Ki(i7));
            x = cat1(x, 0ull);
        }
    }
    an = nn(a);
    if (tp(f) == pf) {
        r = x1(f);
        y10 = x2(f);
        f = r0(f);
        x = stv(r, rx(y10), x);
        a = Drp(a, y10);
    }
    r = l3(f, x, a);
    SetI32(((int32_t)(r)-12), an);
    return ti(pf, (int32_t)(r));
}

static int32_t arity(uint64_t f)
{
    if (tp(f) > df) {
        return nn(f);
    }
    return 2;
}

static uint64_t Cat(uint64_t x, uint64_t y)
{
    int32_t xt, yt;
    {
        xt = tp(x);
        yt = tp(y);
    }
    if ((xt == Tt) && (yt == Dt)) {
        return dcat(x, y);
    }
    if ((xt & 15) == (yt & 15)) {
        if (xt < 16) {
            x = Enl(x);
        }
        return (yt < 16) ? (cat1(x, y)) : (ucat(x, y));
    } else {
        if ((xt == Lt) && (yt < 16)) {
            if (nn(x) > 0) {
                return cat1(x, y);
            }
        }
    }
    x = uf(Cat(explode(x), explode(y)));
    if (!nn(x)) {
        dx(x);
        return mk((xt | 16), 0);
    }
    return x;
}

static uint64_t Enl(uint64_t x) { return uf(l1(x)); }

static uint64_t explode(uint64_t x)
{
    uint64_t r, k8;
    int32_t xt, xn4, rp4, i5, xn8, i9;
    xt = tp(x);
    if ((xt < 16) || (xt == Dt)) {
        return l1(x);
    } else {
        if (xt < Lt) {
            xn4 = nn(x);
            r = mk(Lt, nn(x));
            rp4 = (int32_t)(r);
            {
                i5 = (int32_t)(0);
                for (; (i5 < xn4); i5++)
                    SetI64((rp4 + (8 * i5)), (int64_t)(ati(rx(x), i5)));
            }
            dx(x);
            return r;
        } else {
            if (xt == Tt) {
                xn8 = nn(x);
                k8 = x0(x);
                x = Flp(r1(x));
                r = mk(Lt, 0);
                {
                    i9 = (int32_t)(0);
                    for (; (i9 < xn8); i9++)
                        r = cat1(r, Key(rx(k8), ati(rx(x), i9)));
                }
                dxy(x, k8);
                return r;
            }
        }
    }
    return x;
}

static uint64_t ucat(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xt, xn, yn, s;
    xt = tp(x);
    if (xt > Lt) {
        return dcat(x, y);
    }
    xn = nn(x);
    yn = nn(y);
    r = uspc(x, xt, yn);
    s = sz(xt);
    if (xt == Lt) {
        rl(y);
    }
    Memorycopy(((int32_t)(r) + (s * xn)), (int32_t)(y), (s * yn));
    dx(y);
    return r;
}

static uint64_t dcat(uint64_t x, uint64_t y)
{
    uint64_t r, q;
    int32_t t;
    t = tp(x);
    if (t == Tt) {
        if (!match((uint64_t)(I64((int32_t)(x))),
                   (uint64_t)(I64((int32_t)(y))))) {
            return ucat(explode(x), explode(y));
        }
    }
    r = x0(x);
    x = r1(x);
    q = x0(y);
    y = r1(y);
    return (t == Dt) ? ({
        r = Cat(r, q);
        Key(r, Cat(x, y));
    })
                     : ({
                           dx(q);
                           x = Ech(13ull, l2(x, y));
                           key(r, x, t);
                       });
}

static uint64_t ucat1(uint64_t x, uint64_t y, uint64_t z)
{
    return cat1(ucat(x, y), z);
}

static uint64_t cat1(uint64_t x, uint64_t y)
{
    int32_t t;
    t = tp(x);
    x = uspc(x, t, 1);
    if (t == Lt) {
        y = l1(rx(y));
        x = ti(Ft, (int32_t)(x));
    }
    return ti(t, (int32_t)(sti(x, (nn(x) - 1), y)));
}

static uint64_t uspc(uint64_t x, int32_t xt, int32_t ny)
{
    uint64_t r;
    int32_t nx, s;
    r = (uint64_t)(0ull);
    nx = nn(x);
    s = sz(xt);
    if ((I32(((int32_t)(x)-4)) == 1)
        && (bucket((s * nx)) == bucket((s * (nx + ny))))) {
        r = x;
    } else {
        r = mk(xt, (nx + ny));
        Memorycopy((int32_t)(r), (int32_t)(x), (s * nx));
        if (xt == Lt) {
            rl(x);
        }
        dx(x);
    }
    SetI32(((int32_t)(r)-12), (nx + ny));
    return r;
}

static int32_t quoted(uint64_t x)
{
    return I32B((((int32_t)(x) >= 448) && (tp(x) == 0)));
}

static uint64_t quote(uint64_t x) { return (x + 448ull); }

static uint64_t unquote(uint64_t x) { return (x - 448ull); }

static uint64_t exec(uint64_t x)
{
    uint64_t b, c, a, u4;
    int32_t xn, p, e;
    srcp = 0;
    a = (uint64_t)(0ull);
    xn = nn(x);
    if (!xn) {
        dx(x);
        return 0ull;
    }
    p = (int32_t)(x);
    e = (p + (8 * xn));
    do {
        u4 = (uint64_t)(I64(p));
        if (tp(u4)) {
            push(a);
            a = rx(u4);
        } else {
            switch (((int32_t)(u4) >> 6)) {
            case 0:
                a = ((uint64_t (*)(uint64_t))F_[marksrc(u4)])(a);
                break;
            case 1:
                a = ((uint64_t (*)(uint64_t, uint64_t))F_[marksrc(u4)])(a,
                                                                        pop());
                break;
            case 2: {
                marksrc(a);
                b = pop();
                a = Cal(a, l2(b, pop()));
            } break;
            case 3: {
                b = pop();
                c = pop();
                a = ((uint64_t (*)(uint64_t, uint64_t, uint64_t,
                                   uint64_t))F_[marksrc(u4)])(a, b, c, pop());
            } break;
            case 4: {
                dx(a);
                a = pop();
            } break;
            case 5: {
                p += (int32_t)(a);
                a = pop();
            } break;
            case 6: {
                u4 = pop();
                p += ((int32_t)(a)*I32B(((int32_t)(u4) == 0)));
                dx(u4);
                a = pop();
            } break;
            default: {
                push(a);
                a = rx((u4 - 448ull));
            } break;
            }
        }
        p += 8;
    } while (p < e);
    pop();
    dx(x);
    return a;
}

static int32_t marksrc(uint64_t x)
{
    int32_t p1;
    {
        p1 = h48(x);
        if (p1) {
            srcp = p1;
        }
    }
    return (int32_t)(x);
}

static void push(uint64_t x)
{
    SetI64(sp, (int64_t)(x));
    sp += 8;
    if (sp == 4096) {
        trap();
    }
}

static uint64_t pop(void)
{
    sp = (sp - 8);
    if (sp < 2048) {
        trap();
    }
    return (uint64_t)(I64(sp));
}

static uint64_t lst(uint64_t n)
{
    uint64_t r;
    int32_t rp, e;
    r = mk(Lt, (int32_t)(n));
    rp = (int32_t)(r);
    e = ep(r);
    for (; (rp < e);) {
        SetI64(rp, (int64_t)(pop()));
        rp += 8;
    }
    return uf(r);
}

static uint64_t nul(uint64_t x)
{
    push(x);
    return 0ull;
}

static uint64_t lup(uint64_t x)
{
    int32_t vp;
    vp = (I32(8) + (int32_t)(x));
    return x0((uint64_t)(vp));
}

uint64_t Asn(uint64_t x, uint64_t y)
{
    int32_t vp;
    if (tp(x) != st) {
        trap();
    }
    vp = (I32(8) + (int32_t)(x));
    dx((uint64_t)(I64(vp)));
    SetI64(vp, (int64_t)(rx(y)));
    return y;
}

static uint64_t Amd(uint64_t x, uint64_t i, uint64_t v, uint64_t y)
{
    uint64_t a2, r14;
    int32_t xt, rc3, p4, n10, j11, ti, yt;
    xt = tp(x);
    if (xt == st) {
        a2 = lup(x);
        {
            rc3 = I32(((int32_t)(a2)-4));
            if (rc3 == 2) {
                dx(a2);
                p4 = (int32_t)(a2);
                a2 = rx(Amd(a2, i, v, y));
                if ((int32_t)(a2) != p4) {
                    SetI64((I32(8) + (int32_t)(x)), (int64_t)(a2));
                }
                return a2;
            }
        }
        return Asn(x, Amd(a2, i, v, y));
    }
    if (xt < 16) {
        trap();
    }
    if (tp(i) == Lt) {
        n10 = nn(i);
        {
            j11 = (int32_t)(0);
            for (; (j11 < n10); j11++)
                x = Amd(x, ati(rx(i), j11), rx(v), ati(rx(y), j11));
        }
        dx(i);
        dxy(v, y);
        return x;
    }
    if (xt > Lt) {
        r14 = x0(x);
        x = r1(x);
        if ((xt == Tt) && ((tp(i) & 15) == it)) {
            if (tp(y) > Lt) {
                y = Val(y);
            }
            return key(r14, Dmd(x, l2(0ull, i), v, y), xt);
        }
        r14 = Unq(Cat(r14, rx(i)));
        return key(r14, Amd(ntake(nn(r14), x), Fnd(rx(r14), i), v, y), xt);
    }
    if (i == 0ull) {
        if (v == 1ull) {
            if (tp(y) < 16) {
                y = ntake(nn(x), y);
            }
            dx(x);
            return y;
        }
        return Cal(v, l2(x, y));
    }
    if ((tp(v) != 0) || (v != 1ull)) {
        y = cal(v, l2(Atx(rx(x), rx(i)), y));
    }
    {
        ti = tp(i);
        yt = tp(y);
    }
    if ((xt & 15) != (yt & 15)) {
        x = explode(x);
        xt = Lt;
    }
    if (ti == it) {
        if (xt != (yt + 16)) {
            x = explode(x);
        }
        return sti(use(x), (int32_t)(i), y);
    }
    if (yt < 16) {
        y = ntake(nn(i), y);
        yt = tp(y);
    }
    if (xt == Lt) {
        y = explode(y);
    }
    return stv(x, i, y);
}

static uint64_t Dmd(uint64_t x, uint64_t i, uint64_t v, uint64_t y)
{
    uint64_t f, t12;
    int32_t n8, j15, rj16;
    if (tp(x) == st) {
        return Asn(x, Dmd(lup(x), i, v, y));
    }
    i = explode(i);
    f = Fst(rx(i));
    if (nn(i) == 1) {
        dx(i);
        return Amd(x, f, v, y);
    }
    if (f == 0ull) {
        f = seq(nn(x));
    }
    i = ndrop(1, i);
    if (tp(f) > 16) {
        n8 = nn(f);
        if (nn(i) != 1) {
            trap();
        }
        i = Fst(i);
        if ((tp(f) == It) && (tp(x) == Tt)) {
            t12 = rx(x0(x));
            return key(t12, Dmd(r1(x), l2(Fnd(t12, i), f), v, y), Tt);
        }
        if ((tp(f) != It) || (tp(x) != Lt)) {
            trap();
        }
        x = use(x);
        {
            j15 = (int32_t)(0);
            for (; (j15 < n8); j15++) {
                rj16 = ((int32_t)(x) + (8 * I32(((int32_t)(f) + (4 * j15)))));
                SetI64(rj16,
                       (int64_t)(Amd((uint64_t)(I64(rj16)), rx(i), rx(v),
                                     ati(rx(y), j15))));
            }
        }
        dxy(f, i);
        dxy(v, y);
        return x;
    }
    return Amd(x, f, 1ull, Dmd(Atx(rx(x), f), i, v, y));
}

static uint64_t Fnd(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xt, yt;
    {
        xt = tp(x);
        yt = tp(y);
    }
    if (xt < 16) {
        return (yt == Tt) ? ({
            r = Drp(rx(x), rx(y));
            Atx(r, Grp(Atx(y, x)));
        })
                          : (deal(x, y));
    }
    if (xt > Lt) {
        if (xt == Tt) {
            trap();
        }
        r = x0(x);
        return Atx(r, Fnd(r1(x), y));
    } else {
        if (xt == yt) {
            return Ecr((18ull + (16ull * (uint64_t)(I32B((yt == Lt))))),
                       l2(x, y));
        } else {
            if (xt == (yt + 16)) {
                r = Ki(fnd(x, y, yt));
            } else {
                if (xt == Lt) {
                    return fdl(x, y);
                } else {
                    if (yt == Lt) {
                        return Ecr(18ull, l2(x, y));
                    } else {
                        trap();
                    }
                }
            }
        }
    }
    dxy(x, y);
    return r;
}

static int32_t fnd(uint64_t x, uint64_t y, int32_t t)
{
    int32_t xp, r;
    xp = (int32_t)(x);
    r = ((int32_t (*)(int32_t, int32_t, int32_t))F_[(268 + t)])((int32_t)(y),
                                                                xp, ep(x));
    if (!r) {
        return nn(x);
    }
    return ((r - xp) >> (31 - I32clz(sz((16 + t)))));
}

static uint64_t fdl(uint64_t x, uint64_t y)
{
    int32_t xp, e;
    xp = (int32_t)(x);
    dxy(x, y);
    e = ep(x);
    for (; (xp < e);) {
        if (match((uint64_t)(I64(xp)), y)) {
            return Ki(((xp - (int32_t)(x)) >> 3));
        }
        xp += 8;
    }
    return Ki(nn(x));
}

static int32_t idx(int32_t x, int32_t a, int32_t b)
{
    int32_t i1;
    {
        i1 = a;
        for (; (i1 < b); i1++)
            if (x == I8(i1)) {
                return (i1 - a);
            }
    }
    return -1;
}

static uint64_t Find(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xn, yn, yp, e;
    if ((tp(x) != Ct) || (tp(y) != Ct)) {
        trap();
    }
    {
        xn = nn(x);
        yn = nn(y);
    }
    if (!(xn * yn)) {
        dxy(x, y);
        return mk(It, 0);
    }
    r = mk(It, 0);
    yp = (int32_t)(y);
    e = (((yp + yn) + 1) - xn);
    do {
        if (findat((int32_t)(x), yp, xn)) {
            r = cat1(r, Ki((yp - (int32_t)(y))));
            yp += xn;
        } else {
            yp++;
        }
    } while (yp < e);
    dxy(x, y);
    return r;
}

static int32_t findat(int32_t xp, int32_t yp, int32_t n)
{
    int32_t i1;
    {
        i1 = (int32_t)(0);
        for (; (i1 < n); i1++)
            if (I8((xp + i1)) != I8((yp + i1))) {
                return 0;
            }
    }
    return 1;
}

static uint64_t Mtc(uint64_t x, uint64_t y)
{
    dxy(x, y);
    return Ki(match(x, y));
}

static int32_t match(uint64_t x, uint64_t y)
{
    int32_t xt, n6, xp6, yp6, yn, xp, yp;
    if (x == y) {
        return 1;
    }
    xt = tp(x);
    if (xt != tp(y)) {
        return 0;
    }
    if (xt > 16) {
        n6 = nn(x);
        if (n6 != nn(y)) {
            return 0;
        }
        if (!n6) {
            return 1;
        }
        {
            xp6 = (int32_t)(x);
            yp6 = (int32_t)(y);
        }
        return (xt < Dt)
            ? (((int32_t (*)(int32_t, int32_t, int32_t))F_[(246 + xt)])(
                  xp6, yp6, ep(y)))
            : ({
                  if (match((uint64_t)(I64(xp6)), (uint64_t)(I64(yp6)))) {
                      return match((uint64_t)(I64((xp6 + 8))),
                                   (uint64_t)(I64((yp6 + 8))));
                  }
                  0;
              });
    }
    yn = (int32_t)(0);
    {
        xp = (int32_t)(x);
        yp = (int32_t)(y);
    }
    if (xt < ft) {
        return I32B((xp == yp));
    }
    switch (((int32_t)((xt - ft)) - (3 * I32B((xt > 9))))) {
    case 0:
        return I32B((0 == cmF(xp, yp)));
        break;
    case 1:
        return I32B((0 == cmZ(xp, yp)));
        break;
    case 2:
        yn = (8 * nn(y));
        break;
    case 3:
        yn = 16;
        break;
    case 4:
        yn = 24;
        break;
    case 5:
        return match((uint64_t)(I64((xp + 8))), (uint64_t)(I64((yp + 8))));
        break;
    default:
        return I32B((I64(xp) == I64(yp)));
        break;
    }
    for (; (yn > 0);) {
        yn = (yn - 8);
        if (!match((uint64_t)(I64((xp + yn))), (uint64_t)(I64((yp + yn))))) {
            return 0;
        }
    }
    return 1;
}

static int32_t mtC(int32_t xp, int32_t yp, int32_t e)
{
    int32_t ve;
    ve = (e & ~7);
    for (; (yp < ve);) {
        if (I64(xp) != I64(yp)) {
            return 0;
        }
        xp += 8;
        yp += 8;
    }
    for (; (yp < e);) {
        if (I8(xp) != I8(yp)) {
            return 0;
        }
        xp++;
        yp++;
    }
    return 1;
}

static int32_t mtF(int32_t xp, int32_t yp, int32_t e)
{
    do {
        if (cmF(xp, yp)) {
            return 0;
        }
        xp += 8;
        yp += 8;
    } while (yp < e);
    return 1;
}

static int32_t mtL(int32_t xp, int32_t yp, int32_t e)
{
    do {
        if (!match((uint64_t)(I64(xp)), (uint64_t)(I64(yp)))) {
            return 0;
        }
        xp += 8;
        yp += 8;
    } while (yp < e);
    return 1;
}

static uint64_t In(uint64_t x, uint64_t y)
{
    int32_t xt, yt;
    {
        xt = tp(x);
        yt = tp(y);
    }
    if ((xt == yt) && (xt > 16)) {
        return Ecl(30ull, l2(x, y));
    } else {
        if ((xt + 16) != yt) {
            trap();
        }
    }
    dxy(x, y);
    return Ki(I32B((((int32_t (*)(int32_t, int32_t, int32_t))F_[(268 + xt)])(
                        (int32_t)(x), (int32_t)(y), ep(y))
                    != 0)));
}

static int32_t inC(int32_t x, int32_t yp, int32_t e)
{
    for (; (yp < e);) {
        if (x == I8(yp)) {
            return yp;
        }
        yp++;
    }
    return 0;
}

static int32_t inI(int32_t x, int32_t yp, int32_t e)
{
    for (; (yp < e);) {
        if (x == I32(yp)) {
            return yp;
        }
        yp += 4;
    }
    return 0;
}

static int32_t inF(int32_t xp, int32_t yp, int32_t e)
{
    for (; (yp < e);) {
        if (!cmF(xp, yp)) {
            return yp;
        }
        yp += 8;
    }
    return 0;
}

static int32_t inZ(int32_t xp, int32_t yp, int32_t e)
{
    for (; (yp < e);) {
        if (!cmZ(xp, yp)) {
            return yp;
        }
        yp += 16;
    }
    return 0;
}

uint64_t Atx(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xt, yt, xp;
    {
        xt = tp(x);
        yt = tp(y);
    }
    xp = (int32_t)(x);
    if (xt < 16) {
        if ((xt == 0) || (xt > 9)) {
            return cal(x, l1(y));
        }
        if (xt == st) {
            if (!xp) {
                if (yt == it) {
                    return (uint64_t)((int32_t)(y));
                }
            }
            xt = (ts(x) + 16);
            return ((uint32_t)((xt - 18)) < 5)
                ? (rtp(xt, y))
                : (cal(Val(sc(cat1(cs(x), Kc(46)))), l1(y)));
        }
    }
    if ((xt > Lt) && (yt < Lt)) {
        r = x0(x);
        x = r1(x);
        if (xt == Tt) {
            if ((yt & 15) == it) {
                return key(r, Ecl(19ull, l2(x, y)),
                           (Dt + (int32_t)(I32B((yt == It)))));
            }
        }
        return Atx(x, Fnd(r, y));
    }
    if ((yt & 15) == ft) {
        return Rot(x, y);
    }
    if (yt < It) {
        y = uptype(y, it);
        yt = tp(y);
    }
    if (yt == It) {
        return atv(x, y);
    }
    if (yt == it) {
        return ati(x, (int32_t)(y));
    }
    if (yt == Lt) {
        return Ecr(19ull, l2(x, y));
    }
    if (yt == Dt) {
        r = x0(y);
        return Key(r, Atx(x, r1(y)));
    }
    trap();
    return 0ull;
}

static uint64_t ati(uint64_t x, int32_t i)
{
    uint64_t r;
    int32_t t, s, p;
    r = (uint64_t)(0ull);
    t = tp(x);
    if (t < 16) {
        return x;
    }
    if (t > Lt) {
        return Atx(x, Ki(i));
    }
    if ((i < 0) || (i >= nn(x))) {
        dx(x);
        return missing((t - 16));
    }
    s = sz(t);
    p = ((int32_t)(x) + (i * s));
    switch ((s >> 2)) {
    case 0:
        r = (uint64_t)((uint32_t)(I8(p)));
        break;
    case 1:
        r = (uint64_t)((uint32_t)(I32(p)));
        break;
    case 2:
        r = (uint64_t)((uint64_t)(I64(p)));
        break;
    default: {
        dx(x);
        return Kz(F64(p), F64((p + 8)));
    } break;
    }
    if (t == Ft) {
        r = Kf(F64reinterpret_i64((uint64_t)(r)));
    } else {
        if (t == Lt) {
            r = rx(r);
            dx(x);
            return r;
        }
    }
    dx(x);
    return ti((t - 16), (int32_t)(r));
}

static uint64_t atv(uint64_t x, uint64_t y)
{
    uint64_t r, na;
    int32_t t, yn, xn, s, rp, xp, yp, e, xi8, xi14, xi20, xi29;
    t = tp(x);
    if (t == Tt) {
        return Atx(x, y);
    }
    yn = nn(y);
    if (t < 16) {
        dx(y);
        return ntake(yn, x);
    }
    xn = nn(x);
    r = mk(t, yn);
    s = sz(t);
    rp = (int32_t)(r);
    xp = (int32_t)(x);
    yp = (int32_t)(y);
    e = ep(y);
    na = missing((t - 16));
    switch ((s >> 2)) {
    case 0:
        for (; (yp < e);) {
            xi8 = I32(yp);
            if ((uint32_t)(xi8) >= (uint32_t)(xn)) {
                SetI8(rp, (int32_t)(na));
            } else {
                SetI8(rp, I8((xp + xi8)));
            }
            rp++;
            yp += 4;
        }
        break;
    case 1:
        for (; (yp < e);) {
            xi14 = I32(yp);
            if ((uint32_t)(xi14) >= (uint32_t)(xn)) {
                SetI32(rp, (int32_t)(na));
            } else {
                SetI32(rp, I32((xp + (4 * xi14))));
            }
            rp += 4;
            yp += 4;
        }
        break;
    case 2:
        for (; (yp < e);) {
            xi20 = I32(yp);
            if ((uint32_t)(xi20) >= (uint32_t)(xn)) {
                if (t == Lt) {
                    SetI64(rp, (int64_t)(na));
                } else {
                    SetI64(rp, I64((int32_t)(na)));
                }
            } else {
                SetI64(rp, I64((xp + (8 * xi20))));
            }
            rp += 8;
            yp += 4;
        }
        break;
    default:
        for (; (yp < e);) {
            xi29 = I32(yp);
            if ((uint32_t)(xi29) >= (uint32_t)(xn)) {
                SetI64(rp, I64((int32_t)(na)));
                SetI64((rp + 8), I64((int32_t)(na)));
            } else {
                xi29 = (xi29 * 16);
                SetI64(rp, I64((xp + xi29)));
                SetI64((rp + 8), I64(((8 + xp) + xi29)));
            }
            rp += 16;
            yp += 4;
        }
        break;
    }
    if (t == Lt) {
        rl(r);
        r = uf(r);
    }
    dx(na);
    dxy(x, y);
    return r;
}

static uint64_t stv(uint64_t x, uint64_t i, uint64_t y)
{
    uint64_t j10;
    int32_t n, xt, xn, s, xp, yp, ip, e, j7, j21;
    uint32_t xi8;
    if (It != tp(i)) {
        trap();
    }
    n = nn(i);
    if (!n) {
        dxy(y, i);
        return x;
    }
    if (n != nn(y)) {
        trap();
    }
    x = use(x);
    xt = tp(x);
    xn = nn(x);
    s = sz(xt);
    xp = (int32_t)(x);
    yp = (int32_t)(y);
    ip = (int32_t)(i);
    e = ep(y);
    {
        j7 = (int32_t)(0);
        for (; (j7 < n); j7++) {
            xi8 = (uint32_t)(I32((ip + (4 * j7))));
            if (xi8 >= (uint32_t)(xn)) {
                j10 = rx(Wer(Min(Mor(rx(i), Ki(-1)), Les(rx(i), Ki(xn)))));
                return stv(x, atv(i, j10), atv(y, j10));
            }
        }
    }
    switch ((s >> 2)) {
    case 0:
        for (; (yp < e);) {
            SetI8((xp + I32(ip)), I8(yp));
            ip += 4;
            yp++;
        }
        break;
    case 1:
        for (; (yp < e);) {
            SetI32((xp + (4 * I32(ip))), I32(yp));
            ip += 4;
            yp += 4;
        }
        break;
    case 2: {
        if (xt == Lt) {
            rl(y);
            {
                j21 = (int32_t)(0);
                for (; (j21 < n); j21++) {
                    dx((uint64_t)(I64((xp + (8 * I32(ip))))));
                    ip += 4;
                }
            }
            ip = (int32_t)(i);
        }
        for (; (yp < e);) {
            SetI64((xp + (8 * I32(ip))), I64(yp));
            ip += 4;
            yp += 8;
        }
        if (xt == Lt) {
            x = uf(x);
        }
    } break;
    default:
        for (; (yp < e);) {
            xp = ((int32_t)(x) + (16 * I32(ip)));
            SetI64(xp, I64(yp));
            SetI64((xp + 8), I64((yp + 8)));
            ip += 4;
            yp += 16;
        }
        break;
    }
    dxy(i, y);
    return x;
}

static uint64_t sti(uint64_t x, int32_t i, uint64_t y)
{
    int32_t xt, s, xp, yp;
    xt = tp(x);
    if ((uint32_t)(i) >= (uint32_t)(nn(x))) {
        dx(y);
        return x;
    }
    s = sz(xt);
    xp = (int32_t)(x);
    yp = (int32_t)(y);
    switch ((s >> 2)) {
    case 0:
        SetI8((xp + i), yp);
        break;
    case 1:
        SetI32((xp + (4 * i)), yp);
        break;
    case 2: {
        xp += (8 * i);
        if (xt == Lt) {
            dx((uint64_t)(I64(xp)));
            SetI64(xp, (int64_t)(rx(y)));
            x = uf(x);
        } else {
            SetI64(xp, I64(yp));
        }
    } break;
    default: {
        xp += (16 * i);
        SetI64(xp, I64(yp));
        SetI64((xp + 8), I64((yp + 8)));
    } break;
    }
    dx(y);
    return x;
}

static uint64_t atdepth(uint64_t x, uint64_t y)
{
    uint64_t f;
    int32_t xt;
    xt = tp(x);
    if (xt < 16) {
        trap();
    }
    f = Fst(rx(y));
    if (f == 0ull) {
        f = seq(nn(x));
    }
    x = Atx(x, f);
    if (nn(y) == 1) {
        dx(y);
        return x;
    }
    y = ndrop(1, y);
    if (tp(f) > 16) {
        if ((nn(y) == 1) && (xt == Tt)) {
            return Atx(x, Fst(y));
        }
        return Ecl(20ull, l2(x, y));
    }
    return atdepth(x, y);
}

void kinit(void)
{
    minit(12, 16);
    sp = 2048;
    SetI32(16, (int32_t)(mk(Ct, 0)));
    na = F64reinterpret_i64((uint64_t)(0x7ff8000000000001ull));
    inf = F64reinterpret_i64((uint64_t)(0x7ff0000000000000ull));
    rand_ = 1592653589;
    SetI64(0, (int64_t)(mk(Lt, 0)));
    SetI64(8, (int64_t)(mk(Lt, 0)));
    xyz = sc(Ku(0ull));
    xyz = Ech(17ull, l2(xyz, Ku(8026488ull)));
    zk();
}

static uint64_t ti(int32_t t, int32_t i)
{
    return (((uint64_t)(t) << 59ull) | (uint64_t)((uint32_t)(i)));
}

uint64_t Kc(int32_t x) { return ti(ct, x); }

uint64_t Ki(int32_t x) { return ti(it, x); }

static uint64_t Ks(int32_t x) { return ti(st, x); }

uint64_t Kf(double x)
{
    uint64_t r;
    r = mk(Ft, 1);
    SetF64((int32_t)(r), x);
    return ti(ft, (int32_t)(r));
}

static uint64_t Kz(double x, double y)
{
    uint64_t r;
    int32_t rp;
    r = mk(Zt, 1);
    rp = (int32_t)(r);
    SetF64(rp, x);
    SetF64((rp + 8), y);
    return ti(zt, rp);
}

static uint64_t l1(uint64_t x)
{
    uint64_t r;
    r = mk(Lt, 1);
    SetI64((int32_t)(r), (int64_t)(x));
    return r;
}

uint64_t l2(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t rp;
    r = mk(Lt, 2);
    rp = (int32_t)(r);
    SetI64(rp, (int64_t)(x));
    SetI64((8 + rp), (int64_t)(y));
    return r;
}

static uint64_t l3(uint64_t x, uint64_t y, uint64_t z)
{
    return cat1(l2(x, y), z);
}

static uint64_t r0(uint64_t x)
{
    uint64_t r;
    r = x0(x);
    dx(x);
    return r;
}

static uint64_t r1(uint64_t x)
{
    uint64_t r;
    r = x1(x);
    dx(x);
    return r;
}

static uint64_t x0(uint64_t x) { return rx((uint64_t)(I64((int32_t)(x)))); }

static uint64_t x1(uint64_t x) { return x0((x + 8ull)); }

static uint64_t x2(uint64_t x) { return x0((x + 16ull)); }

static uint64_t Ku(uint64_t x)
{
    uint64_t r;
    int32_t p;
    r = mk(Ct, 8);
    p = (int32_t)(r);
    SetI64(p, (int64_t)(x));
    SetI32((p - 12), idx(0, p, (p + 8)));
    return r;
}

static uint64_t kx(int32_t u, uint64_t x) { return cal(Val(Ks(u)), l1(x)); }

uint64_t sc(uint64_t c)
{
    uint64_t s;
    int32_t sp, sn, i1;
    s = (uint64_t)(I64(0));
    sp = (int32_t)(s);
    sn = nn(s);
    {
        i1 = (int32_t)(0);
        for (; (i1 < sn); i1++) {
            if (match(c, (uint64_t)(I64(sp)))) {
                dx(c);
                return ti(st, (sp - (int32_t)(s)));
            }
            sp += 8;
        }
    }
    SetI64(0, (int64_t)(cat1(s, c)));
    SetI64(8, (int64_t)(cat1((uint64_t)(I64(8)), 0ull)));
    return ti(st, (8 * sn));
}

uint64_t cs(uint64_t x) { return x0(((uint64_t)(I32(0)) + x)); }

static uint64_t missing(int32_t t)
{
    switch ((t - 2)) {
    case 0:
        return Kc(32);
        break;
    case 1:
        return Ki(0);
        break;
    case 2:
        return Ks(0);
        break;
    case 3:
        return Kf(na);
        break;
    case 4:
        return Kz(na, na);
        break;
    default:
        return mk(Ct, 0);
        break;
    }
}

void main_(void)
{
    uint64_t x2;
    kinit();
    doargs();
    write(Ku(0x000a6b2f6579746bull));
    store();
    for (;;) {
        write(Ku(32ull));
        x2 = readfile(mk(Ct, 0));
        try(x2);
    }
}

static void store(void)
{
    int32_t g;
    g = ((1 << (I32(128) - 16)) - Memorysize2());
    if (g > 0) {
        Memorygrow2(g);
    }
    Memorycopy2(0, 0, ((int32_t)(1) << I32(128)));
}

static void catch(void)
{
    Memorycopy3(0, 0, ((int32_t)(65536) * Memorysize2()));
}

static void try(uint64_t x)
{
    jb__ = 1;
    if (!setjmp(jb_)) {
        ;
        repl(x);
        store();
    } else {
        catch();
    }
}

static void doargs(void)
{
    uint64_t a, ee, x2;
    int32_t an, i1;
    a = ndrop(1, getargv());
    an = nn(a);
    ee = Ku(25901ull);
    {
        i1 = (int32_t)(0);
        for (; (i1 < an); i1++) {
            x2 = x0(a);
            if (match(x2, ee)) {
                if (i1 < (an - 1)) {
                    dx(x2);
                    x2 = x1(a);
                    dx(ee);
                    repl(x2);
                }
                Exit(0);
            }
            dofile(x2, readfile(rx(x2)));
            a += 8ull;
        }
    }
    dx(ee);
}

static void dofile(uint64_t x, uint64_t c)
{
    uint64_t kk, tt, xe;
    kk = Ku(27438ull);
    tt = Ku(29742ull);
    xe = ntake(-2, rx(x));
    if (match(xe, kk)) {
        dx(val(c));
    } else {
        if (match(xe, tt)) {
            test(c);
        } else {
            dx(Asn(sc(rx(x)), c));
        }
    }
    dxy(xe, x);
    dxy(tt, kk);
}

static uint64_t getargv(void)
{
    uint64_t r, s2;
    int32_t n, rp, i1;
    n = Args();
    r = mk(Lt, n);
    rp = (int32_t)(r);
    {
        i1 = (int32_t)(0);
        for (; (i1 < n); i1++) {
            s2 = mk(Ct, Arg(i1, 0));
            Arg(i1, (int32_t)(s2));
            SetI64(rp, (int64_t)(s2));
            rp += 8;
        }
    }
    return r;
}

static double hypot_(double p, double q)
{
    double t2;
    {
        p = F64abs(p);
        q = F64abs(q);
    }
    if (p < q) {
        t2 = p;
        p = q;
        q = t2;
    }
    if (p == 0.) {
        return 0.;
    }
    q = (q / p);
    return (p * F64sqrt((1. + (q * q))));
}

static void cosin(double deg, int32_t rp)
{
    double c, s;
    {
        c = 0.;
        s = 0.;
    }
    if (deg == 0.) {
        c = 1.;
    } else {
        if (deg == 90.) {
            s = 1.;
        } else {
            if (deg == 180.) {
                c = -1.;
            } else {
                if (deg == 270.) {
                    s = -1.;
                } else {
                    cosin_((deg * 0.017453292519943295), rp, 0);
                    return;
                }
            }
        }
    }
    SetF64(rp, c);
    SetF64((rp + 8), s);
}

static double ang2(double y, double x)
{
    double deg;
    if (y == 0.) {
        if (x < 0.) {
            return 180.;
        }
        return 0.;
    }
    if (x == 0.) {
        if (y < 0.) {
            return 270.;
        }
        return 90.;
    }
    deg = (57.29577951308232 * atan2_(y, x));
    if (deg < 0.) {
        deg += 360.;
    }
    return deg;
}

static void exp1(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, exp_(F64(xp)));
}

static void log1(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, log_(F64(xp)));
}

static void pow2(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, pow_(F64(xp), F64(yp)));
}

static void sin1(int32_t xp, int32_t yp, int32_t rp)
{
    cosin_(F64(xp), rp, 1);
}

static void cos1(int32_t xp, int32_t yp, int32_t rp)
{
    cosin_(F64(xp), rp, 2);
}

static void cosin_(double x, int32_t rp, int32_t csonly)
{
    int32_t ss, cs;
    double c, s, y, z, zz;
    int64_t j;
    {
        c = 0.;
        s = 0.;
        ss = (int32_t)(0);
        cs = (int32_t)(0);
    }
    if (x < 0.) {
        x = -x;
        ss = 1;
    }
    j = (int64_t)((x * 1.2732395447351628));
    y = (double)(j);
    if ((j & 1ll) == 1ll) {
        j += 1ll;
        y += 1.;
    }
    j = (j & 7ll);
    z = (((x - (y * 0.7853981256484985)) - (y * 3.774894707930798e-08))
         - (y * 2.6951514290790595e-15));
    if (j > 3ll) {
        j = (j - 4ll);
        {
            ss = (1 - ss);
            cs = (1 - cs);
        }
    }
    if (j > 1ll) {
        cs = (1 - cs);
    }
    zz = (z * z);
    c = ((1. - (0.5 * zz))
         + ((zz * zz)
            * ((((((((((-1.1358536521387682e-11 * zz) + 2.087570084197473e-09)
                      * zz)
                     + -2.755731417929674e-07)
                    * zz)
                   + 2.4801587288851704e-05)
                  * zz)
                 + -0.0013888888888873056)
                * zz)
               + 0.041666666666666595)));
    s = (z
         + ((z * zz)
            * ((((((((((1.5896230157654656e-10 * zz) + -2.5050747762857807e-08)
                      * zz)
                     + 2.7557313621385722e-06)
                    * zz)
                   + -0.0001984126982958954)
                  * zz)
                 + 0.008333333333322118)
                * zz)
               + -0.1666666666666663)));
    if ((j == 1ll) || (j == 2ll)) {
        x = c;
        c = s;
        s = x;
    }
    if (cs) {
        c = -c;
    }
    if (ss) {
        s = -s;
    }
    SetF64(rp, c);
    if (!csonly) {
        SetF64((rp + 8), s);
    } else {
        if (csonly == 1) {
            SetF64(rp, s);
        }
    }
}

static double atan2_(double y, double x)
{
    double q;
    q = atan_((y / x));
    if (x < 0.) {
        if (q <= 0.) {
            return (q + pi);
        }
        return (q - pi);
    }
    return q;
}

static double atan_(double x) { return (x > 0.) ? (satan(x)) : (-satan(-x)); }

static double satan(double x)
{
    if (x <= 0.66) {
        return xatan(x);
    }
    if (x > 2.414213562373095) {
        return ((1.5707963267948966 - xatan((1. / x)))
                + 6.123233995736766e-17);
    }
    return ((0.7853981633974483 + xatan(((x - 1.) / (x + 1.))))
            + (0.5 * 6.123233995736766e-17));
}

static double xatan(double x)
{
    double z;
    z = (x * x);
    z = ((z
          * ((((((((-0.8750608600031904 * z) + -16.157537187333652) * z)
                 + -75.00855792314705)
                * z)
               + -122.88666844901361)
              * z)
             + -64.85021904942025))
         / (((((((((z + 24.858464901423062) * z) + 165.02700983169885) * z)
                + 432.88106049129027)
               * z)
              + 485.3903996359137)
             * z)
            + 194.5506571482614));
    z = ((x * z) + x);
    return z;
}

static double exp_(double x)
{
    double hi, lo;
    int64_t k;
    if (x != x) {
        return x;
    }
    if (x > 709.782712893384) {
        return inf;
    }
    if (x < -745.1332191019411) {
        return 0.;
    }
    if ((-3.725290298461914e-09 < x) && (x < 3.725290298461914e-09)) {
        return (1. + x);
    }
    k = (x < 0.) ? ((int64_t)(((1.4426950408889634 * x) - 0.5)))
                 : ((int64_t)(((1.4426950408889634 * x) + 0.5)));
    hi = (x - ((double)(k) * 0.6931471803691238));
    lo = ((double)(k) * 1.9082149292705877e-10);
    return expmulti(hi, lo, k);
}

static double expmulti(double hi, double lo, int64_t k)
{
    double r, t, c, y;
    r = (hi - lo);
    t = (r * r);
    c = (r
         - (t
            * (0.16666666666666666
               + (t
                  * (-0.0027777777777015593
                     + (t
                        * (6.613756321437934e-05
                           + (t
                              * (-1.6533902205465252e-06
                                 + (t * 4.1381367970572385e-08))))))))));
    y = (1. - ((lo - ((r * c) / (2. - c))) - hi));
    return ldexp_(y, k);
}

static double ldexp_(double frac, int64_t exp)
{
    uint64_t x;
    double nf, m;
    if ((((frac == 0.) || (frac > maxfloat)) || (frac < -maxfloat))
        || (frac != frac)) {
        return frac;
    }
    nf = normalize(frac);
    if (nf != frac) {
        exp = (exp - 52ll);
        frac = nf;
    }
    x = (uint64_t)(I64reinterpret_f64(frac));
    exp += (((int64_t)((x >> 52ull)) & 2047ll) - 1023ll);
    if (exp < (int64_t)(-1075ll)) {
        return F64copysign(0., frac);
    }
    if (exp > (int64_t)(1023ll)) {
        if (frac < 0.) {
            return -inf;
        }
        return inf;
    }
    m = 1.;
    if (exp < (int64_t)(-1022ll)) {
        exp += 53ll;
        m = 1.1102230246251565e-16;
    }
    x = (x & ~0x7ff0000000000000ull);
    x = (x | ((uint64_t)((exp + 1023ll)) << 52ull));
    return (m * F64reinterpret_i64((uint64_t)(x)));
}

static int32_t frexp1(double f)
{
    if (f == 0.) {
        return 0;
    }
    if (((f < -maxfloat) || (f > maxfloat)) || (f != f)) {
        return 0;
    }
    return 1;
}

static double frexp2(double f)
{
    uint64_t x;
    f = normalize(f);
    x = I64reinterpret_f64(f);
    x = (x & ~0x7ff0000000000000ull);
    x = (x | 0x3fe0000000000000ull);
    return F64reinterpret_i64(x);
}

static int64_t frexp3(double f)
{
    uint64_t x;
    double nf;
    int64_t exp;
    exp = (int64_t)(0ll);
    nf = normalize(f);
    if (nf != f) {
        exp = (int64_t)(-52ll);
        f = nf;
    }
    x = I64reinterpret_f64(f);
    return ((exp + (int64_t)(((x >> 52ull) & 2047ull))) - 1022ll);
}

static double normalize(double x)
{
    if (F64abs(x) < 2.2250738585072014e-308) {
        return (x * 4.503599627370496e+15);
    }
    return x;
}

static double log_(double x)
{
    double f1, f, k, s, s2, s4, t1, t2, R, hfsq;
    int64_t ki;
    if ((x != x) || (x > maxfloat)) {
        return x;
    }
    if (x < 0.) {
        return na;
    }
    if (x == 0.) {
        return -inf;
    }
    f1 = x;
    ki = (int64_t)(0ll);
    if (frexp1(x)) {
        f1 = frexp2(x);
        ki = frexp3(x);
    }
    if (f1 < 0.7071067811865476) {
        f1 = (f1 * 2.);
        ki = (ki - 1ll);
    }
    f = (f1 - 1.);
    k = (double)(ki);
    s = (f / (2. + f));
    s2 = (s * s);
    s4 = (s2 * s2);
    t1 = (s2
          * (0.6666666666666735
             + (s4
                * (0.2857142874366239
                   + (s4
                      * (0.1818357216161805 + (s4 * 0.14798198605116586)))))));
    t2 = (s4
          * (0.3999999999940942
             + (s4 * (0.22222198432149784 + (s4 * 0.15313837699209373)))));
    R = (t1 + t2);
    hfsq = ((0.5 * f) * f);
    return (
        (k * 0.6931471803691238)
        - ((hfsq - ((s * (hfsq + R)) + (k * 1.9082149292705877e-10))) - f));
}

static double modabsfi(double f)
{
    uint64_t x, e;
    if (f < 1.) {
        return 0.;
    }
    x = I64reinterpret_f64(f);
    e = (((x >> 52ull) & 2047ull) - 1023ull);
    if (e < 52ull) {
        x = (x & ~(((uint64_t)(1ull) << (52ull - e)) - (uint64_t)(1ull)));
    }
    return F64reinterpret_i64(x);
}

static double pow_(double x, double y)
{
    double yf, yi, a1, x1;
    int64_t ae, xe, i31;
    if ((y == 0.) || (x == 1.)) {
        return 1.;
    }
    if (y == 1.) {
        return x;
    }
    if ((((x != x) || (y != y)) || (y > maxfloat)) || (y < -maxfloat)) {
        return na;
    }
    if (x == 0.) {
        return (y < 0.) ? (inf) : (0.);
    }
    if (y == 0.5) {
        return F64sqrt(x);
    }
    if (y == -0.5) {
        return (1. / F64sqrt(x));
    }
    yf = F64abs(y);
    yi = modabsfi(yf);
    yf = (yf - yi);
    if ((yf != 0.) && (x < 0.)) {
        return na;
    }
    if (yi >= 9.223372036854776e+18) {
        if (x == -1.) {
            return 1.;
        } else {
            return ((F64abs(x) < 1.) == (y > 0.)) ? (0.) : (inf);
        }
    }
    a1 = 1.;
    ae = (int64_t)(0ll);
    if (yf != 0.) {
        if (yf > 0.5) {
            yf = (yf - 1.);
            yi += 1.;
        }
        a1 = exp_((yf * log_(x)));
    }
    x1 = x;
    xe = (int64_t)(0ll);
    if (frexp1(x)) {
        x1 = frexp2(x);
        xe = frexp3(x);
    }
    {
        i31 = (int64_t)(yi);
        for (; (i31 != 0ll); i31 = (i31 >> (int64_t)(1ll))) {
            if ((xe < (int64_t)(-4096ll)) || (4096ll < xe)) {
                ae += xe;
                break;
            }
            if ((i31 & 1ll) == 1ll) {
                a1 = (a1 * x1);
                ae += xe;
            }
            x1 = (x1 * x1);
            xe = (xe << (int64_t)(1ll));
            if (x1 < 0.5) {
                x1 += x1;
                xe = (xe - 1ll);
            }
        }
    }
    if (y < 0.) {
        a1 = (1. / a1);
        ae = -ae;
    }
    return ldexp_(a1, ae);
}

static uint64_t ipow(uint64_t x, int32_t y)
{
    return (tp(x) == It) ? (Ecr(42ull, l2(Ki(y), x)))
                         : (Ki(iipow((int32_t)(x), y)));
}

static int32_t iipow(int32_t x, int32_t y)
{
    int32_t r;
    r = (int32_t)(1);
    for (;;) {
        if ((y & 1) == 1) {
            r = (r * x);
        }
        y = (y >> 1);
        if (!y) {
            break;
        }
        x = (x * x);
    }
    return r;
}

static uint64_t Prs(uint64_t x) { return parse(Tok(x)); }

static uint64_t parse(uint64_t x)
{
    uint64_t r;
    int32_t n;
    if (tp(x) != Lt) {
        trap();
    }
    pp = (int32_t)(x);
    n = (8 * nn(x));
    pe = (n + pp);
    r = es();
    if (pp != pe) {
        srcp = ps;
        trap();
    }
    mfree(((int32_t)(x)-vl), bucket(n));
    return r;
}

static uint64_t es(void)
{
    uint64_t r, n2, x2;
    r = mk(Lt, 0);
    for (;;) {
        n2 = next();
        if (n2 == 0ull) {
            break;
        }
        if (n2 == 59ull) {
            continue;
        }
        pp = (pp - 8);
        x2 = (e(t()) & ~1ull);
        if (x2 == 0ull) {
            break;
        }
        if (nn(r)) {
            r = cat1(r, 256ull);
        }
        r = Cat(r, x2);
    }
    return r;
}

static uint64_t e(uint64_t x)
{
    uint64_t r, xv, y, yv, ev6, a6, ev;
    int32_t xs;
    xv = (x & 1ull);
    x = (x & ~1ull);
    if (x == 0ull) {
        return 0ull;
    }
    xs = ps;
    y = t();
    yv = (y & 1ull);
    y = (y & ~1ull);
    if (y == 0ull) {
        return (x + xv);
    }
    if ((yv != 0ull) && (xv == 0ull)) {
        r = e(t());
        ev6 = (r & 1ull);
        r = (r & ~1ull);
        a6 = pasn(x, y, r);
        if (a6 != 0ull) {
            return a6;
        }
        if ((r == 0ull) || (ev6 == 1ull)) {
            x = ucat1(cat1(ucat1(l1(0ull), x, Ki(2)), 27ull), y, 92ull);
            if (ev6 == 1ull) {
                return (ucat1(r, x, 91ull) + 1ull);
            }
            return (x + 1ull);
        }
        return dyadic(ucat(r, x), y);
    }
    r = e((rx(y) + yv));
    ev = (r & 1ull);
    r = (r & ~1ull);
    dx(y);
    if (xv == 0ull) {
        return ucat1(r, x, (83ull | ((uint64_t)(xs) << 32ull)));
    } else {
        if (((r == y) && ((xv + yv) == 2ull)) || (ev == 1ull)) {
            return (ucat1(r, x, 91ull) + 1ull);
        }
    }
    return idiom(monadic(ucat(r, x)));
}

static uint64_t t(void)
{
    uint64_t r, verb, n25, ks25, p31, s31;
    int32_t rt, a25;
    r = next();
    if (r == 0ull) {
        return 0ull;
    }
    rt = tp(r);
    if ((rt == 0) && ((int32_t)(r) < 127)) {
        if (is((int32_t)(r), 32)) {
            pp = (pp - 8);
            return 0ull;
        }
    }
    verb = (uint64_t)(0ull);
    if (r == (uint64_t)(40ull)) {
        r = rlist((plist(41ull) & ~1ull), 0ull);
    } else {
        if (r == (uint64_t)(123ull)) {
            r = plam(ps);
        } else {
            if (r == (uint64_t)(91ull)) {
                r = es();
                if (next() != (uint64_t)(93ull)) {
                    srcp = ps;
                    trap();
                }
                return r;
            } else {
                r = (rt == st)
                    ? (l2(r, (20ull | ((uint64_t)(ps) << 32ull))))
                    : ({
                          if (!rt) {
                              r = (quote(r) | ((uint64_t)(ps) << 32ull));
                              verb = 1ull;
                          } else {
                              if (rt == St) {
                                  if (nn(r) == 1) {
                                      r = Fst(r);
                                  }
                              }
                          }
                          l1(r);
                      });
            }
        }
    }
    for (;;) {
        n25 = next();
        if (n25 == 0ull) {
            break;
        }
        ks25 = ((uint64_t)(ps) << 32ull);
        a25 = (int32_t)(n25);
        if (((tp(n25) == 0) && (a25 > 20)) && (a25 < 27)) {
            r = cat1(r, n25);
            verb = 1ull;
        } else {
            if (n25 == 91ull) {
                verb = 0ull;
                n25 = plist(93ull);
                p31 = ((uint64_t)(84ull) + (8ull * (n25 & 1ull)));
                n25 = (n25 & ~1ull);
                s31 = pspec(r, n25);
                if (s31 != 0ull) {
                    return s31;
                }
                r = (nn(n25) == 1)
                    ? (ucat1(Fst(n25), r, (83ull | ks25)))
                    : (cat1(Cat(rlist(n25, 2ull), r), (p31 | ks25)));
            } else {
                pp = (pp - 8);
                break;
            }
        }
    }
    return (r + verb);
}

static uint64_t pasn(uint64_t x, uint64_t y, uint64_t r)
{
    uint64_t l, lp4;
    int32_t v, sp, xn2;
    l = (uint64_t)(I64((int32_t)(y)));
    v = (int32_t)(l);
    sp = h48(l);
    if ((((nn(y) == 1) && (tp(l) == 0)) && (v == 449))
        || ((v > 544) && (v < 565))) {
        dx(y);
        xn2 = nn(x);
        if (xn2 > 2) {
            if (v > 544) {
                l = (l - 96ull);
            }
            lp4 = (0xff000000ffffffffull & lastp(x));
            if (lp4 == 92ull) {
                lp4 = 84ull;
            }
            x = ucat1(l1(l), ldrop(-2, x),
                      (((uint64_t)(sp) << 32ull) | (lp4 + 128ull)));
            r = ucat(r, x);
            return r;
        } else {
            y = ((v == 449) || (v == 545))
                ? ({
                      x = Fst(x);
                      if (xn2 == 1) {
                          if (tp(x) == st) {
                              x = sc(cat1(cs(Fst(x)), Kc(46)));
                          } else {
                              dx(x);
                              return r;
                          }
                      }
                      if ((loc != 0ull) && (v == 449)) {
                          loc = Cat(loc, rx(x));
                      }
                      x = l1(x);
                      l1(448ull);
                  })
                : (cat1(l2(unquote((l - 32ull)), Fst(rx(x))), 448ull));
        }
        return dyadic(ucat(r, x), y);
    }
    return 0ull;
}

static uint64_t plam(int32_t s0)
{
    uint64_t r, slo, n, n2, c, i, s;
    int32_t ar, ln2, cn, cp, y15;
    r = (uint64_t)(0ull);
    slo = loc;
    loc = 0ull;
    ar = (int32_t)(-1);
    n = next();
    if (n == 91ull) {
        n2 = (plist(93ull) & ~1ull);
        ln2 = nn(n2);
        loc = Ech(4ull, l1(n2));
        if ((ln2 > 0) && (tp(loc) != St)) {
            srcp = ps;
            trap();
        }
        ar = nn(loc);
        if (!ar) {
            dx(loc);
            loc = mk(St, 0);
        }
    } else {
        pp = (pp - 8);
        loc = mk(St, 0);
    }
    c = es();
    n = next();
    if (n != 125ull) {
        srcp = ps;
        trap();
    }
    cn = nn(c);
    cp = (int32_t)(c);
    if (ar < 0) {
        ar = 0;
        for (; (cn > 0);) {
            cn = (cn - 1);
            r = (uint64_t)(I64(cp));
            if ((tp(r) == 0) && ((int32_t)(r) == 20)) {
                r = (uint64_t)(I64((cp - 8)));
                y15 = ((int32_t)(r) >> 3);
                if (((tp(r) == st) && (y15 > 0)) && (y15 < 4)) {
                    ar = maxi(ar, y15);
                }
            }
            cp += 8;
        }
        loc = Cat(ntake(ar, rx(xyz)), loc);
    }
    i = Add(seq(((1 + ps) - s0)), Ki((s0 - 1)));
    s = atv(rx(src()), i);
    r = l3(c, s, Unq(loc));
    loc = slo;
    cp = (int32_t)(r);
    SetI32((cp - 12), ar);
    return l1((ti(lf, cp) | ((uint64_t)(s0) << 32ull)));
}

static uint64_t pspec(uint64_t r, uint64_t n)
{
    uint64_t v;
    int32_t ln;
    ln = nn(n);
    v = (uint64_t)(I64((int32_t)(r)));
    if ((nn(r) == 1) && (ln > 2)) {
        if ((tp(v) == 0) && ((int32_t)(v) == 465)) {
            dx(r);
            return cond(n, ln);
        }
    }
    if (((nn(r) == 2) && (ln > 1)) && ((int32_t)(v) == 64)) {
        dx(r);
        return whl(n, (ln - 1));
    }
    return 0ull;
}

static uint64_t whl(uint64_t x, int32_t xn)
{
    uint64_t r, y2;
    int32_t p, xp, sum, i1;
    r = cat1(Fst(rx(x)), 0ull);
    p = (nn(r) - 1);
    r = ucat(r, l2(384ull, 256ull));
    xp = (int32_t)(x);
    sum = (int32_t)(2);
    {
        i1 = (int32_t)(0);
        for (; (i1 < xn); i1++) {
            if (i1) {
                r = cat1(r, 256ull);
            }
            xp += 8;
            y2 = x0((uint64_t)(xp));
            sum += (1 + nn(y2));
            r = ucat(r, y2);
        }
    }
    r = cat1(cat1(r, Ki((-8 * (2 + nn(r))))), 320ull);
    SetI64(((int32_t)(r) + (8 * p)), (int64_t)(Ki((8 * sum))));
    dx(x);
    return ucat(l1(0ull), r);
}

static uint64_t cond(uint64_t x, int32_t xn)
{
    uint64_t r2;
    int32_t nxt, sum, xp, state;
    nxt = (int32_t)(0);
    sum = (int32_t)(0);
    xp = ((int32_t)(x) + (8 * xn));
    state = (int32_t)(1);
    for (; (xp != (int32_t)(x));) {
        xp = (xp - 8);
        r2 = (uint64_t)(I64(xp));
        if (r2 == 0ull) {
            r2 = l1(r2);
        }
        if (sum > 0) {
            state = (1 - state);
            r2 = (state) ? (cat1(cat1(r2, Ki(nxt)), 384ull))
                         : (cat1(cat1(r2, Ki(sum)), 320ull));
            SetI64(xp, (int64_t)(r2));
        }
        nxt = (8 * nn(r2));
        sum += nxt;
    }
    return Rdc(13ull, l1(x));
}

static uint64_t plist(uint64_t c)
{
    uint64_t p, r, b2, x2;
    p = (uint64_t)(0ull);
    r = mk(Lt, 0);
    for (;;) {
        b2 = next();
        if ((b2 == 0ull) || (b2 == c)) {
            break;
        }
        if (!nn(r)) {
            pp = (pp - 8);
        }
        x2 = (e(t()) & ~1ull);
        if (x2 == 0ull) {
            p = 1ull;
        }
        r = cat1(r, x2);
    }
    return (r + p);
}

static uint64_t rlist(uint64_t x, uint64_t p)
{
    int32_t n;
    n = nn(x);
    if (!n) {
        return l1(x);
    }
    if (n == 1) {
        return Fst(x);
    }
    if (p != 2ull) {
        p = clist(x);
        if (p != 0ull) {
            return l1(p);
        }
    }
    return cat1(cat1(Rdc(13ull, l1(Rev(x))), Ki(n)), 27ull);
}

static uint64_t clist(uint64_t x)
{
    uint64_t xi2;
    int32_t p, e, t2;
    p = (int32_t)(x);
    e = ep(x);
    for (; (p < e);) {
        xi2 = (uint64_t)(I64(p));
        t2 = tp(xi2);
        if (t2 != Lt) {
            return 0ull;
        }
        if (nn(xi2) != 1) {
            return 0ull;
        }
        if (!tp((uint64_t)(I64((int32_t)(xi2))))) {
            return 0ull;
        }
        p += 8;
    }
    return uf(Rdc(13ull, l1(x)));
}

static uint64_t next(void)
{
    uint64_t r;
    if (pp == pe) {
        return 0ull;
    }
    r = (uint64_t)(I64(pp));
    ps = h48(r);
    pp += 8;
    return (r & 0xff000000ffffffffull);
}

static uint64_t lastp(uint64_t x) { return (uint64_t)(I64((ep(x) - 8))); }

static int32_t h48(uint64_t x) { return (16777215 & (int32_t)((x >> 32ull))); }

static uint64_t dyadic(uint64_t x, uint64_t y)
{
    uint64_t l;
    l = lastp(y);
    if (quoted(l)) {
        return ucat1(x, ldrop(-1, y), (64ull + unquote(l)));
    }
    return ucat1(x, y, 128ull);
}

static uint64_t monadic(uint64_t x)
{
    uint64_t l;
    l = lastp(x);
    if (quoted(l)) {
        x = ldrop(-1, x);
        return ((int32_t)(l) == 449) ? (cat1(cat1(x, Ki(1048576)), 320ull))
                                     : (cat1(x, unquote(l)));
    }
    return cat1(x, 83ull);
}

static uint64_t ldrop(int32_t n, uint64_t x) { return explode(ndrop(n, x)); }

static int32_t svrb(int32_t p)
{
    uint64_t x;
    x = (uint64_t)(I64(p));
    return (I32B((((int32_t)(x) < 64) && (tp(x) == 0))) * (int32_t)(x));
}

static uint64_t idiom(uint64_t x)
{
    int32_t l, i;
    l = ((int32_t)(x) + (8 * (nn(x) - 2)));
    i = (svrb(l) + (svrb((l + 8)) << 6));
    if ((i == 262) || (i == 263)) {
        i = 34;
    } else {
        return x;
    }
    SetI64(l, (I64(l) + (int64_t)(i)));
    return ndrop(-1, x);
}

static int32_t rnd(void)
{
    int32_t r;
    r = rand_;
    r = (r ^ (r << 13));
    r = (r ^ (r >> 17));
    r = (r ^ (r << 5));
    rand_ = r;
    return r;
}

static uint64_t roll(uint64_t x)
{
    uint64_t r5;
    int32_t xt, xp;
    xt = tp(x);
    xp = (int32_t)(x);
    if (xt == it) {
        return (xp > 0) ? (kx(72, x)) : ({
            r5 = kx(80, Ki(((1 + -xp) / 2)));
            SetI32(((int32_t)(r5)-12), -xp);
            ti(Ft, (int32_t)(r5));
        });
    }
    if (xt == zt) {
        dx(x);
        return kx(80, Ki((int32_t)(F64floor(F64(xp)))));
    }
    trap();
    return 0ull;
}

static uint64_t deal(uint64_t x, uint64_t y)
{
    int32_t yt, xp, yp;
    yt = tp(y);
    if (yt > 16) {
        return In(x, y);
    }
    if (tp(x) != it) {
        trap();
    }
    xp = (int32_t)(x);
    if (yt == ct) {
        return Add(Kc(97), Flr(deal(x, Ki(((int32_t)(y)-96)))));
    }
    if (yt == st) {
        return Ech(17ull, l2(Ks(0), deal(x, Fst(cs(y)))));
    }
    if (yt != it) {
        trap();
    }
    yp = (int32_t)(y);
    if (xp > 0) {
        return randI(yp, xp);
    }
    return ntake(-xp, shuffle(seq(yp), -xp));
}

static int32_t randi(int32_t n)
{
    uint64_t prod;
    uint32_t v, low, thresh2;
    v = (uint32_t)(rnd());
    prod = ((uint64_t)(v) * (uint64_t)(n));
    low = (uint32_t)(prod);
    if (low < (uint32_t)(n)) {
        thresh2 = ((uint32_t)(-n) % (uint32_t)(n));
        for (; (low < thresh2);) {
            v = (uint32_t)(rnd());
            prod = ((uint64_t)(v) * (uint64_t)(n));
            low = (uint32_t)(prod);
        }
    }
    return (int32_t)((prod >> 32ull));
}

static uint64_t randI(int32_t i, int32_t n)
{
    uint64_t r;
    int32_t rp, e;
    r = mk(It, n);
    rp = (int32_t)(r);
    e = ep(r);
    if (!i) {
        for (; (rp < e);) {
            SetI32(rp, rnd());
            rp += 4;
        }
    } else {
        for (; (rp < e);) {
            SetI32(rp, randi(i));
            rp += 4;
        }
    }
    return r;
}

static uint64_t shuffle(uint64_t r, int32_t m)
{
    int32_t rp, n, i1, j2, t2;
    rp = (int32_t)(r);
    n = nn(r);
    m = mini((n - 1), m);
    {
        i1 = (int32_t)(0);
        for (; (i1 < m); i1++) {
            j2 = (rp + (4 * randi((n - i1))));
            t2 = I32(rp);
            SetI32(rp, I32(j2));
            SetI32(j2, t2);
            rp += 4;
        }
    }
    return r;
}

static uint64_t rd0(int32_t yp, int32_t t, int32_t n) { return 0ull; }

static uint64_t min(int32_t yp, int32_t t, int32_t e)
{
    int32_t xp;
    switch ((t - 18)) {
    case 0: {
        xp = 127;
        for (; (yp < e);) {
            xp = mini(xp, I8(yp));
            yp++;
        }
        return Kc(xp);
    } break;
    case 1:
        return Ki(minis(2147483647, yp, e));
        break;
    case 2:
        return Ks(minis(((nn((uint64_t)(I64(8))) << 3) - 8), yp, e));
        break;
    case 3:
        return Kf(minfs(yp, e));
        break;
    default:
        return 0ull;
        break;
    }
}

static uint64_t max(int32_t yp, int32_t t, int32_t e)
{
    int32_t xp;
    switch ((t - 18)) {
    case 0: {
        xp = -128;
        for (; (yp < e);) {
            xp = maxi(xp, I8(yp));
            yp++;
        }
        return Kc(xp);
    } break;
    case 1:
        return Ki(maxis((int32_t)0x80000000, yp, e));
        break;
    case 2:
        return Ks(maxis(0, yp, e));
        break;
    case 3:
        return Kf(maxfs(yp, e));
        break;
    default:
        return 0ull;
        break;
    }
}

static uint64_t sum(int32_t yp, int32_t t, int32_t e)
{
    uint64_t r8;
    int32_t xp;
    double f7;
    xp = (int32_t)(0);
    switch ((t - 18)) {
    case 0: {
        for (; (yp < e);) {
            xp += I8(yp);
            yp++;
        }
        return Kc(xp);
    } break;
    case 1:
        return Ki((xp + sumi(yp, e)));
        break;
    case 2:
        return 0ull;
        break;
    case 3: {
        f7 = 0.;
        return Kf((f7 + sumf(yp, e)));
    } break;
    case 4: {
        r8 = Kz(0., 0.);
        sumz(yp, e, (int32_t)(r8));
        return r8;
    } break;
    default:
        return 0ull;
        break;
    }
}

static uint64_t prd(int32_t yp, int32_t t, int32_t e)
{
    int32_t xp;
    double f9;
    xp = (int32_t)(1);
    switch ((t - 18)) {
    case 0: {
        for (; (yp < e);) {
            xp = (xp * I8(yp));
            yp++;
        }
        return Kc(xp);
    } break;
    case 1: {
        for (; (yp < e);) {
            xp = (xp * I32(yp));
            yp += 4;
        }
        return Ki(xp);
    } break;
    case 2:
        return 0ull;
        break;
    case 3: {
        f9 = 1.;
        for (; (yp < e);) {
            f9 = (f9 * F64(yp));
            yp += 8;
        }
        return Kf(f9);
    } break;
    default:
        return 0ull;
        break;
    }
}

static uint64_t Neg(uint64_t x) { return nm(220, x); }

static int32_t negi(int32_t x) { return -x; }

static double negf(double x) { return -x; }

static uint64_t negz(double x, double y) { return Kz(-x, -y); }

static uint64_t Abs(uint64_t x)
{
    int32_t xt, xp4;
    xt = tp(x);
    if (xt > Zt) {
        return Ech(32ull, l1(x));
    }
    if (xt == zt) {
        xp4 = (int32_t)(x);
        dx(x);
        return Kf(hypot_(F64(xp4), F64((xp4 + 8))));
    } else {
        if (xt == Zt) {
            return absZ(x);
        }
    }
    return nm(223, x);
}

static int32_t absi(int32_t x)
{
    if (x < 0) {
        return -x;
    }
    return x;
}

static double absf(double x) { return F64abs(x); }

static uint64_t absZ(uint64_t x)
{
    uint64_t r;
    int32_t n, rp, xp;
    n = nn(x);
    r = mk(Ft, n);
    rp = (int32_t)(r);
    xp = (int32_t)(x);
    do {
        n = (n - 1);
        SetF64(rp, hypot_(F64(xp), F64((xp + 8))));
        xp += 16;
        rp += 8;
    } while (n > 0);
    dx(x);
    return r;
}

static uint64_t Sqr(uint64_t x)
{
    if ((tp(x) & 15) != ft) {
        x = Add(Kf(0.), x);
    }
    return nm(244, x);
}

static double sqrf(double x) { return F64sqrt(x); }

static uint64_t Hyp(uint64_t x, uint64_t y)
{
    int32_t xt, yt, xp6, yp6;
    xt = tp(x);
    yt = tp(y);
    if ((xt > Zt) || (yt > Zt)) {
        return Ech(32ull, l2(x, y));
    }
    if (xt == zt) {
        x = Abs(x);
        xt = ft;
    }
    if (xt == ft) {
        xp6 = (int32_t)(x);
        yp6 = (int32_t)(y);
        dxy(x, y);
        if (yt == ft) {
            return Kf(hypot_(F64(xp6), F64(yp6)));
        } else {
            if (yt == zt) {
                return Kf(hypot_(F64(xp6), hypot_(F64(yp6), F64((yp6 + 8)))));
            }
        }
    }
    trap();
    return 0ull;
}

static uint64_t Img(uint64_t x)
{
    int32_t xt;
    xt = tp(x);
    if (xt > Zt) {
        return Ech(33ull, l1(x));
    }
    if (xt == Zt) {
        return reim(x, 8);
    }
    dx(x);
    if (xt == zt) {
        return Kf(F64(((int32_t)(x) + 8)));
    }
    return (xt < zt) ? (Kf(0.)) : (ntake(nn(x), Kf(0.)));
}

static uint64_t Cpx(uint64_t x, uint64_t y)
{
    return Add(x, Mul(Kz(0., 1.), y));
}

static uint64_t Cnj(uint64_t x)
{
    int32_t xt, xp, e;
    xt = tp(x);
    if (xt > Zt) {
        return Ech(34ull, l1(x));
    }
    if ((xt & 15) < zt) {
        return x;
    }
    xp = (int32_t)(x);
    if (tp(x) == zt) {
        dx(x);
        return Kz(F64(xp), -F64((xp + 8)));
    }
    x = use(x);
    xp = (8 + (int32_t)(x));
    e = ep(x);
    for (; (xp < e);) {
        SetF64(xp, -F64(xp));
        xp += 16;
    }
    return x;
}

static uint64_t Add(uint64_t x, uint64_t y) { return nd(226, 2, x, y); }

static int32_t addi(int32_t x, int32_t y) { return (x + y); }

static void addf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) + F64(yp)));
}

static void addz(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) + F64(yp)));
    SetF64((rp + 8), (F64((xp + 8)) + F64((yp + 8))));
}

static uint64_t Sub(uint64_t x, uint64_t y) { return nd(238, 3, x, y); }

static int32_t subi(int32_t x, int32_t y) { return (x - y); }

static void subf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) - F64(yp)));
}

static void subz(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) - F64(yp)));
    SetF64((rp + 8), (F64((xp + 8)) - F64((yp + 8))));
}

static uint64_t Mul(uint64_t x, uint64_t y) { return nd(232, 4, x, y); }

static int32_t muli(int32_t x, int32_t y) { return (x * y); }

static void mulf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) * F64(yp)));
}

static void mulz(int32_t xp, int32_t yp, int32_t rp)
{
    double xr, xi, yr, yi;
    {
        xr = F64(xp);
        xi = F64((xp + 8));
    }
    {
        yr = F64(yp);
        yi = F64((yp + 8));
    }
    SetF64(rp, ((xr * yr) - (xi * yi)));
    SetF64((rp + 8), ((xr * yi) + (xi * yr)));
}

static uint64_t Mod(uint64_t x, uint64_t y) { return nd(244, 41, x, y); }

static int32_t modi(int32_t x, int32_t y)
{
    if (!y) {
        return x;
    }
    x = (x % y);
    return (x + (y * I32B((x < 0))));
}

static uint64_t Div(uint64_t x, uint64_t y) { return nd(241, 5, x, y); }

static int32_t divi(int32_t x, int32_t y)
{
    if (!y) {
        return x;
    }
    return ((x - ((y - 1) * I32B((x < 0)))) / y);
}

static void divf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, (F64(xp) / F64(yp)));
}

static void divz(int32_t xp, int32_t yp, int32_t rp)
{
    double xr, xi, yr, yi, r, d, e, f;
    {
        xr = F64(xp);
        xi = F64((xp + 8));
    }
    {
        yr = F64(yp);
        yi = F64((yp + 8));
    }
    {
        r = 0.;
        d = 0.;
        e = 0.;
        f = 0.;
    }
    f = (F64abs(yr) >= F64abs(yi)) ? ({
        r = (yi / yr);
        d = (yr + (r * yi));
        e = ((xr + (xi * r)) / d);
        ((xi - (xr * r)) / d);
    })
                                   : ({
                                         r = (yr / yi);
                                         d = (yi + (r * yr));
                                         e = (((xr * r) + xi) / d);
                                         (((xi * r) - xr) / d);
                                     });
    SetF64(rp, e);
    SetF64((rp + 8), f);
}

static uint64_t Min(uint64_t x, uint64_t y) { return nd(229, 6, x, y); }

static int32_t mini(int32_t x, int32_t y)
{
    if (x < y) {
        return x;
    }
    return y;
}

static void minf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, F64min(F64(xp), F64(yp)));
}

static void minz(int32_t xp, int32_t yp, int32_t rp)
{
    if (cmZ(xp, yp) > 0) {
        xp = yp;
    }
    SetI64(rp, I64(xp));
    SetI64((rp + 8), I64((xp + 8)));
}

static uint64_t Max(uint64_t x, uint64_t y) { return nd(235, 7, x, y); }

static int32_t maxi(int32_t x, int32_t y) { return (x > y) ? (x) : (y); }

static void maxf(int32_t xp, int32_t yp, int32_t rp)
{
    SetF64(rp, F64max(F64(xp), F64(yp)));
}

static void maxz(int32_t xp, int32_t yp, int32_t rp)
{
    if (cmZ(xp, yp) < 0) {
        xp = yp;
    }
    SetI64(rp, I64(xp));
    SetI64((rp + 8), I64((xp + 8)));
}

static int32_t cmi(int32_t x, int32_t y)
{
    return (I32B((x > y)) - I32B((x < y)));
}

static int32_t cmC(int32_t x, int32_t y)
{
    {
        x = I8(x);
        y = I8(y);
    }
    return (I32B((x > y)) - I32B((x < y)));
}

static int32_t cmI(int32_t x, int32_t y)
{
    {
        x = I32(x);
        y = I32(y);
    }
    return (I32B((x > y)) - I32B((x < y)));
}

static int32_t cmF(int32_t x, int32_t y)
{
    int64_t a, b;
    {
        a = I64(x);
        b = I64(y);
    }
    if (2 == (I32B((a < 0ll)) + I32B((b < 0ll)))) {
        a = -a;
        b = -b;
    }
    return (I32B((a > b)) - I32B((a < b)));
}

static int32_t cmZ(int32_t x, int32_t y)
{
    int32_t r;
    r = cmF(x, y);
    return (!r) ? (cmF((x + 8), (y + 8))) : (r);
}

static uint64_t Eql(uint64_t x, uint64_t y) { return nc(10, 0, x, y); }

static uint64_t Les(uint64_t x, uint64_t y)
{
    if ((tp(x) == st) && (tp(y) == Ct)) {
        if (!(int32_t)(x)) {
            write(rx(y));
            return y;
        }
        return writefile(cs(x), y);
    }
    return nc(8, -1, x, y);
}

static uint64_t Mor(uint64_t x, uint64_t y) { return nc(9, 1, x, y); }

static uint64_t Ang(uint64_t x)
{
    uint64_t r;
    int32_t xt, xp, n, rp8, e8;
    xt = tp(x);
    if (xt > Zt) {
        return Ech(35ull, l1(x));
    }
    if (xt < zt) {
        dx(x);
        return Kf(0.);
    }
    xp = (int32_t)(x);
    if (xt == zt) {
        dx(x);
        return Kf(ang2(F64((xp + 8)), F64(xp)));
    }
    n = nn(x);
    if (xt == Zt) {
        r = mk(Ft, n);
        rp8 = (int32_t)(r);
        e8 = (rp8 + (8 * n));
        for (; (rp8 < e8);) {
            SetF64(rp8, ang2(F64((xp + 8)), F64(xp)));
            xp += 16;
            rp8 += 8;
        }
    } else {
        r = ntake(n, Kf(0.));
    }
    dx(x);
    return r;
}

static uint64_t Rot(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t yp, yn9, rp9;
    if (tp(x) > Zt) {
        return Ech(35ull, l2(x, y));
    }
    x = uptype(x, zt);
    if (y == 0ull) {
        return x;
    }
    if ((tp(y) & 15) > ft) {
        trap();
    }
    y = uptype(y, ft);
    yp = (int32_t)(y);
    if (tp(y) == ft) {
        r = Kz(0., 0.);
        cosin(F64(yp), (int32_t)(r));
    } else {
        yn9 = nn(y);
        r = mk(Zt, yn9);
        rp9 = (int32_t)(r);
        for (; (yn9 > 0);) {
            yn9 = (yn9 - 1);
            cosin(F64(yp), rp9);
            yp += 8;
            rp9 += 16;
        }
    }
    dx(y);
    return Mul(r, x);
}

static uint64_t Sin(uint64_t x) { return nf(44, x, 0ull); }

static uint64_t Cos(uint64_t x) { return nf(45, x, 0ull); }

static uint64_t Exp(uint64_t x) { return nf(42, x, 0ull); }

static uint64_t Log(uint64_t x) { return nf(43, x, 0ull); }

static uint64_t Pow(uint64_t y, uint64_t x)
{
    if ((tp(x) & 15) == it) {
        if (tp(y) == it) {
            if ((int32_t)(y) >= 0) {
                return ipow(x, (int32_t)(y));
            }
        }
    }
    return nf(106, x, y);
}

static uint64_t Lgn(uint64_t x, uint64_t y)
{
    double xf;
    xf = fk(x);
    if (xf == 10.) {
        xf = 0.4342944819032518;
    } else {
        xf = (xf == 2.) ? (1.4426950408889634) : ((1. / log_(xf)));
    }
    return Mul(Kf(xf), Log(y));
}

static double fk(uint64_t x)
{
    int32_t t;
    t = tp(x);
    if (t == it) {
        return (double)((int32_t)(x));
    }
    if (t != ft) {
        trap();
    }
    dx(x);
    return F64((int32_t)(x));
}

static uint64_t nf(int32_t f, uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xt, xp, xn, dr14, e14;
    xt = tp(x);
    if (xt >= Lt) {
        return (y == 0ull) ? (Ech((uint64_t)(f), l1(x)))
                           : (Ech((uint64_t)((f - 64)), l2(y, x)));
    }
    if ((xt & 15) < ft) {
        x = uptype(x, ft);
        xt = tp(x);
    }
    if ((tp(y) < ft) && (y != 0ull)) {
        y = uptype(y, ft);
    }
    xp = (int32_t)(x);
    xn = (int32_t)(1);
    r = (xt == ft) ? (Kf(0.)) : ({
        xn = nn(x);
        mk(Ft, xn);
    });
    if (xn > 0) {
        f += (233 - (60 * I32B((f == 106))));
        dr14 = ((int32_t)(r)-xp);
        e14 = (xp + (8 * xn));
        do {
            ((void (*)(int32_t, int32_t, int32_t))F_[f])(xp, (int32_t)(y),
                                                         (xp + dr14));
            xp += 8;
        } while (xp < e14);
    }
    dxy(x, y);
    return r;
}

static int32_t conform(uint64_t x, uint64_t y)
{
    int32_t r;
    r = ((2 * I32B((tp(x) > 16))) + I32B((tp(y) > 16)));
    if (r == 3) {
        if (nn(x) != nn(y)) {
            trap();
        }
    }
    return r;
}

static int32_t dtypes(uint64_t x, uint64_t y)
{
    int32_t xt, yt;
    {
        xt = tp(x);
        yt = tp(y);
    }
    return (int32_t)(maxi((int32_t)(xt), (int32_t)(yt)));
}

static uint64_t dkeys(uint64_t x, uint64_t y)
{
    if (tp(x) > Lt) {
        return x0(x);
    }
    return x0(y);
}

static uint64_t dvals(uint64_t x)
{
    if (tp(x) > Lt) {
        return r1(x);
    }
    return x;
}

static int32_t maxtype(uint64_t x, uint64_t y)
{
    int32_t xt, yt, t;
    {
        xt = (tp(x) & 15);
        yt = (tp(y) & 15);
    }
    t = (int32_t)(maxi((int32_t)(xt), (int32_t)(yt)));
    if (!t) {
        t = it;
    }
    return t;
}

static uint64_t uptype(uint64_t x, int32_t dst)
{
    uint64_t r;
    int32_t xt, xp, xn, rp, e;
    double f10;
    xt = tp(x);
    xp = (int32_t)(x);
    if ((xt & 15) == dst) {
        return x;
    }
    if (xt < 16) {
        if (dst < st) {
            return ti(dst, xp);
        } else {
            if (dst == ft) {
                return Kf((double)(xp));
            } else {
                return (dst == zt) ? ({
                    f10 = (double)(xp);
                    if (xt == ft) {
                        f10 = F64(xp);
                        dx(x);
                    }
                    Kz(f10, 0.);
                })
                                   : ({
                                         trap();
                                         0ull;
                                     });
            }
        }
    }
    if ((xt < It) && (dst == ft)) {
        x = uptype(x, it);
        xt = It;
    }
    if ((xt < Ft) && (dst == zt)) {
        x = uptype(x, ft);
        xt = Ft;
    }
    xn = nn(x);
    xp = (int32_t)(x);
    r = mk((dst + 16), xn);
    rp = (int32_t)(r);
    e = ep(r);
    if (dst == it) {
        for (; (rp < e);) {
            SetI32(rp, I8(xp));
            xp++;
            rp += 4;
        }
    } else {
        if (dst == ft) {
            for (; (rp < e);) {
                SetF64(rp, (double)(I32(xp)));
                xp += 4;
                rp += 8;
            }
        } else {
            if (dst == zt) {
                for (; (rp < e);) {
                    SetF64(rp, F64(xp));
                    SetF64((rp + 8), 0.);
                    xp += 8;
                    rp += 16;
                }
            } else {
                trap();
            }
        }
    }
    dx(x);
    return r;
}

static uint64_t use1(uint64_t x)
{
    if (I32(((int32_t)(x)-4)) == 1) {
        return rx(x);
    }
    return mk(tp(x), nn(x));
}

static uint64_t use(uint64_t x)
{
    uint64_t r;
    int32_t xt, nx;
    xt = tp(x);
    if ((xt < 16) || (xt > Lt)) {
        trap();
    }
    if (I32(((int32_t)(x)-4)) == 1) {
        return x;
    }
    nx = nn(x);
    r = mk(xt, nx);
    Memorycopy((int32_t)(r), (int32_t)(x), (sz(xt) * nx));
    if (xt == Lt) {
        rl(r);
    }
    dx(x);
    return r;
}

static uint64_t seq(int32_t n)
{
    uint64_t r;
    n = maxi(n, 0);
    r = mk(It, n);
    for (; (n > 0);) {
        n = (n - 1);
        SetI32(((int32_t)(r) + (4 * n)), n);
    }
    return r;
}

static int32_t minis(int32_t x, int32_t y, int32_t e)
{
    for (; (y < e);) {
        x = mini(x, I32(y));
        y += 4;
    }
    return x;
}

static int32_t maxis(int32_t x, int32_t y, int32_t e)
{
    for (; (y < e);) {
        x = maxi(x, I32(y));
        y += 4;
    }
    return x;
}

static double minfs(int32_t y, int32_t e)
{
    double f;
    f = inf;
    for (; (y < e);) {
        f = F64min(f, F64(y));
        y += 8;
    }
    return f;
}

static double maxfs(int32_t y, int32_t e)
{
    double f;
    f = -inf;
    for (; (y < e);) {
        f = F64max(f, F64(y));
        y += 8;
    }
    return f;
}

static int32_t sumi(int32_t xp, int32_t e)
{
    int32_t r;
    r = (int32_t)(0);
    for (; (xp < e);) {
        r += I32(xp);
        xp += 4;
    }
    return r;
}

static double sumf(int32_t xp, int32_t e)
{
    double r;
    r = 0.;
    for (; (xp < e);) {
        r += F64(xp);
        xp += 8;
    }
    return r;
}

static void sumz(int32_t xp, int32_t e, int32_t rp)
{
    double r, i;
    r = 0.;
    i = 0.;
    for (; (xp < e);) {
        r += F64(xp);
        xp += 8;
        i += F64(xp);
        xp += 8;
    }
    SetF64(rp, r);
    SetF64((rp + 8), i);
}

static uint64_t nm(int32_t f, uint64_t x)
{
    uint64_t r;
    int32_t xt, xp, n4, rp4, e;
    xt = tp(x);
    if (xt > Lt) {
        r = x0(x);
        return key(r, nm(f, r1(x)), xt);
    }
    xp = (int32_t)(x);
    if (xt == Lt) {
        n4 = nn(x);
        r = mk(Lt, n4);
        rp4 = (int32_t)(r);
        for (; (n4 > 0);) {
            n4 = (n4 - 1);
            SetI64(rp4, (int64_t)(nm(f, x0((uint64_t)(xp)))));
            xp += 8;
            rp4 += 8;
        }
        dx(x);
        return uf(r);
    }
    if (xt < 16) {
        switch ((xt - 2)) {
        case 0:
            return Kc(((int32_t (*)(int32_t))F_[f])(xp));
            break;
        case 1:
            return Ki(((int32_t (*)(int32_t))F_[f])(xp));
            break;
        case 2: {
            trap();
            return 0ull;
        } break;
        case 3: {
            r = Kf(((double (*)(double))F_[(1 + f)])(F64(xp)));
            dx(x);
            return r;
        } break;
        case 4: {
            r = ((uint64_t (*)(double, double))F_[(2 + f)])(F64(xp),
                                                            F64((xp + 8)));
            dx(x);
            return r;
        } break;
        default: {
            trap();
            return 0ull;
        } break;
        }
    }
    x = use(x);
    xp = (int32_t)(x);
    e = ep(x);
    if (e == xp) {
        return x;
    }
    switch ((xt - 18)) {
    case 0:
        do {
            SetI8(xp, ((int32_t (*)(int32_t))F_[f])(I8(xp)));
            xp++;
        } while (xp < e);
        break;
    case 1:
        do {
            SetI32(xp, ((int32_t (*)(int32_t))F_[f])(I32(xp)));
            xp += 4;
        } while (xp < e);
        break;
    case 2:
        trap();
        break;
    default:
        do {
            SetF64(xp, ((double (*)(double))F_[(1 + f)])(F64(xp)));
            xp += 8;
        } while (xp < e);
        break;
    }
    return x;
}

static uint64_t nd(int32_t f, int32_t ff, uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t n, t, av, xp, yp, ix, iy, rp, e, dz;
    t = dtypes(x, y);
    if (t > Lt) {
        r = dkeys(x, y);
        return key(r,
                   ((uint64_t (*)(uint64_t, uint64_t))F_[(64 + ff)])(dvals(x),
                                                                     dvals(y)),
                   t);
    }
    if (t == Lt) {
        return Ech((uint64_t)(ff), l2(x, y));
    }
    t = maxtype(x, y);
    x = uptype(x, t);
    y = uptype(y, t);
    av = conform(x, y);
    {
        xp = (int32_t)(x);
        yp = (int32_t)(y);
    }
    if (!av) {
        switch ((t - 2)) {
        case 0:
            return Kc(((int32_t (*)(int32_t, int32_t))F_[f])(xp, yp));
            break;
        case 1:
            return Ki(((int32_t (*)(int32_t, int32_t))F_[f])(xp, yp));
            break;
        case 2: {
            trap();
            return 0ull;
        } break;
        default: {
            r = mk((16 + t), 1);
            dxy(x, y);
            ((void (*)(int32_t, int32_t,
                       int32_t))F_[((f - 4) + (int32_t)(t))])(xp, yp,
                                                              (int32_t)(r));
            return Fst(r);
        } break;
        }
    }
    ix = sz((t + 16));
    iy = ix;
    if (av == 1) {
        x = Enl(x);
        xp = (int32_t)(x);
        ix = 0;
        n = nn(y);
        r = use1(y);
    } else {
        if (av == 2) {
            n = nn(x);
            y = Enl(y);
            yp = (int32_t)(y);
            iy = 0;
            r = use1(x);
        } else {
            n = nn(x);
            r = (I32(((int32_t)(y)-4)) == 1) ? (rx(y)) : (use1(x));
        }
    }
    if (!n) {
        dxy(x, y);
        return r;
    }
    rp = (int32_t)(r);
    e = ep(r);
    dz = ((int32_t)(8) << I32B((t > ft)));
    switch ((t - 2)) {
    case 0:
        do {
            SetI8(rp, ((int32_t (*)(int32_t, int32_t))F_[f])(I8(xp), I8(yp)));
            xp += ix;
            yp += iy;
            rp++;
        } while (rp < e);
        break;
    case 1:
        do {
            SetI32(rp,
                   ((int32_t (*)(int32_t, int32_t))F_[f])(I32(xp), I32(yp)));
            xp += ix;
            yp += iy;
            rp += 4;
        } while (rp < e);
        break;
    case 2:
        trap();
        break;
    default:
        do {
            ((void (*)(int32_t, int32_t,
                       int32_t))F_[((f - 4) + (int32_t)(t))])(xp, yp, rp);
            xp += ix;
            yp += iy;
            rp += dz;
        } while (rp < e);
        break;
    }
    dxy(x, y);
    return r;
}

static uint64_t nc(int32_t ff, int32_t q, uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t n, t, av, xp, yp, ix, iy, rp, e;
    t = dtypes(x, y);
    if (t > Lt) {
        r = dkeys(x, y);
        return key(r, nc(ff, q, dvals(x), dvals(y)), t);
    }
    if (t == Lt) {
        return Ech((uint64_t)(ff), l2(x, y));
    }
    t = maxtype(x, y);
    x = uptype(x, t);
    y = uptype(y, t);
    av = conform(x, y);
    {
        xp = (int32_t)(x);
        yp = (int32_t)(y);
    }
    if (!av) {
        dxy(x, y);
        return Ki(I32B(
            (q == ((int32_t (*)(int32_t, int32_t))F_[(245 + t)])(xp, yp))));
    }
    ix = sz((t + 16));
    iy = ix;
    if (av == 1) {
        x = Enl(x);
        xp = (int32_t)(x);
        ix = 0;
        n = nn(y);
    } else {
        if (av == 2) {
            n = nn(x);
            y = Enl(y);
            yp = (int32_t)(y);
            iy = 0;
        } else {
            n = nn(x);
        }
    }
    r = mk(It, n);
    if (!n) {
        dxy(x, y);
        return r;
    }
    rp = (int32_t)(r);
    e = ep(r);
    do {
        SetI32(
            rp,
            I32B((q
                  == ((int32_t (*)(int32_t, int32_t))F_[(250 + t)])(xp, yp))));
        xp += ix;
        yp += iy;
        rp += 4;
    } while (rp < e);
    dxy(x, y);
    return r;
}

static uint64_t Srt(uint64_t x)
{
    uint64_t r, i6;
    int32_t xt;
    xt = tp(x);
    if (xt < 16) {
        if (xt == it) {
            x = seq((int32_t)(x));
            return Div(x, Kf((double)(nn(x))));
        }
        trap();
    }
    if (xt == Dt) {
        r = x0(x);
        x = r1(x);
        i6 = rx(Asc(rx(x)));
        return Key(atv(r, i6), atv(x, i6));
    }
    if (nn(x) < 2) {
        return x;
    }
    return atv(x, Asc(rx(x)));
}

static uint64_t Asc(uint64_t x)
{
    int32_t t;
    t = tp(x);
    if (t == st) {
        return readfile(cs(x));
    } else {
        if (t == it) {
            return mat(8ull, x);
        }
    }
    return grade(x, 1);
}

static uint64_t Dsc(uint64_t x)
{
    if (tp(x) == it) {
        return mat(9ull, x);
    }
    return grade(x, -1);
}

static uint64_t grade(uint64_t x, int32_t f)
{
    uint64_t r, w;
    int32_t xt, n, rp, xp, wp;
    xt = tp(x);
    if (xt < 16) {
        trap();
    }
    if (xt == Dt) {
        r = x0(x);
        return Atx(r, grade(r1(x), f));
    }
    n = nn(x);
    if (xt == Tt) {
        return cal(lup(Ks(88)), l2(x, Ki(I32B((f == -1)))));
    }
    if (n < 2) {
        dx(x);
        return seq(n);
    }
    r = seq(n);
    rp = (int32_t)(r);
    xp = (int32_t)(x);
    w = mk(It, n);
    wp = (int32_t)(w);
    Memorycopy(wp, rp, (4 * n));
    msrt(wp, rp, 0, n, xp, (int32_t)(xt), f);
    dxy(w, x);
    return r;
}

static void msrt(int32_t x, int32_t r, int32_t a, int32_t b, int32_t p,
                 int32_t t, int32_t f)
{
    int32_t c;
    if ((b - a) < 2) {
        return;
    }
    c = ((a + b) >> 1);
    msrt(r, x, a, c, p, t, f);
    msrt(r, x, c, b, p, t, f);
    mrge(x, r, (4 * a), (4 * b), (4 * c), p, t, f);
}

static void mrge(int32_t x, int32_t r, int32_t a, int32_t b, int32_t c,
                 int32_t p, int32_t t, int32_t f)
{
    int32_t q, i, j, s, k1;
    {
        i = a;
        j = c;
    }
    s = sz((int32_t)(t));
    {
        k1 = a;
        for (; (k1 < b); k1 += 4) {
            q = ((i < c) && (j < b))
                ? (I32B((f
                         == ((int32_t (*)(int32_t, int32_t))F_[(234 + t)])(
                             (p + (s * I32((x + i)))),
                             (p + (s * I32((x + j))))))))
                : (0);
            if ((i >= c) || (q != 0)) {
                SetI32((r + k1), I32((x + j)));
                j += 4;
            } else {
                SetI32((r + k1), I32((x + i)));
                i += 4;
            }
        }
    }
}

static int32_t cmL(int32_t xp, int32_t yp)
{
    uint64_t x, y;
    int32_t r, xt, yt, xp4, yp4, xp6, yp6, xn, yn, n, s, e;
    {
        x = (uint64_t)(I64(xp));
        y = (uint64_t)(I64(yp));
    }
    {
        xt = tp(x);
        yt = tp(y);
    }
    if (xt != yt) {
        return (I32B((xt > yt)) - I32B((xt < yt)));
    }
    if (xt < 16) {
        {
            xp4 = (int32_t)(x);
            yp4 = (int32_t)(y);
        }
        return ((int32_t (*)(int32_t, int32_t))F_[(245 + xt)])(xp4, yp4);
    }
    if (xt > Lt) {
        {
            xp6 = (int32_t)(x);
            yp6 = (int32_t)(y);
        }
        r = cmL(xp6, yp6);
        if (!r) {
            r = cmL((xp6 + 8), (yp6 + 8));
        }
        return r;
    }
    {
        xn = nn(x);
        yn = nn(y);
    }
    xp = (int32_t)(x);
    yp = ((int32_t)(y)-xp);
    n = mini(xn, yn);
    s = sz(xt);
    e = (xp + (n * s));
    for (; (xp < e);) {
        r = ((int32_t (*)(int32_t, int32_t))F_[(234 + xt)])(xp, (xp + yp));
        if (r) {
            return r;
        }
        xp += s;
    }
    return (I32B((xn > yn)) - I32B((xn < yn)));
}

static uint64_t Kst(uint64_t x) { return Atx(Ks(32), x); }

static uint64_t Lst(uint64_t x) { return Atx(Ks(40), x); }

static uint64_t Str(uint64_t x)
{
    uint64_t r, f8, l8, i8;
    int32_t xt, xp, ft8, ip15;
    xt = tp(x);
    if (xt > 16) {
        return Ech(17ull, l1(x));
    }
    xp = (int32_t)(x);
    if (xt > 8) {
        switch ((xt - cf)) {
        case 0: {
            rx(x);
            r = Rdc(13ull, l1(Rev(Str(ti(Lt, xp)))));
        } break;
        case 1:
            r = ucat(Str(x0(x)), Str((21ull + x1(x))));
            break;
        case 2: {
            f8 = x0(x);
            l8 = x1(x);
            i8 = x2(x);
            ft8 = tp(f8);
            f8 = Str(f8);
            dx(i8);
            r = (((nn(i8) == 1) && (I32((int32_t)(i8)) == 1))
                 && ((ft8 == 0) || (ft8 == df)))
                ? (ucat(Kst(Fst(l8)), f8))
                : (ucat(f8, emb(91, 93, ndrop(-1, ndrop(1, Kst(l8))))));
        } break;
        default:
            r = x1(x);
            break;
        }
        dx(x);
        return r;
    } else {
        switch (xt) {
        case 0: {
            if (xp > 448) {
                return Str(((uint64_t)(xp)-448ull));
            }
            ip15 = xp;
            switch ((xp >> 6)) {
            case 0:
                if (!xp) {
                    return mk(Ct, 0);
                }
                break;
            case 1:
                ip15 = (ip15 - 64);
                break;
            case 2:
                ip15 = (ip15 - 128);
                break;
            default:
                ip15 = (ip15 - 192);
                break;
            }
            if ((ip15 > 25) || (ip15 == 0)) {
                return ucat(Ku(96ull), si(xp));
            }
            r = Ku((uint64_t)(I8((226 + ip15))));
        } break;
        case 1:
            r = 0ull;
            break;
        case 2:
            r = Ku((uint64_t)(xp));
            break;
        case 3:
            r = si(xp);
            break;
        case 4:
            r = cs(x);
            break;
        case 5:
            r = sf(F64(xp));
            break;
        default:
            r = sfz(F64(xp), F64((xp + 8)));
            break;
        }
    }
    dx(x);
    return r;
}

static uint64_t emb(int32_t a, int32_t b, uint64_t x)
{
    return cat1(Cat(Kc(a), x), Kc(b));
}

static uint64_t si(int32_t x)
{
    uint64_t r;
    if (!x) {
        return Ku((uint64_t)(48ull));
    } else {
        if (x == (int32_t)0x80000000) {
            return ucat(Ku(0x000034373431322dull), Ku(0x0000003834363338ull));
        } else {
            if (x < 0) {
                return ucat(Ku((uint64_t)(45ull)), si(-x));
            }
        }
    }
    r = mk(Ct, 0);
    for (; (x);) {
        r = cat1(r, Kc((48 + (x % 10))));
        x = (x / 10);
    }
    return Rev(r);
}

static uint64_t sf(double x)
{
    uint64_t u, r;
    int32_t c, i15, n, rp;
    int64_t i;
    c = (int32_t)(0);
    if (x != x) {
        return Ku(28208ull);
    }
    u = (uint64_t)(I64reinterpret_f64(x));
    if (u == (uint64_t)(I64reinterpret_f64(inf))) {
        return Ku(30512ull);
    } else {
        if (u == (uint64_t)(I64reinterpret_f64(-inf))) {
            return Ku(7811117ull);
        }
    }
    if (x < 0.) {
        return ucat(Ku((uint64_t)(45ull)), sf(-x));
    }
    if ((x > 0.) && ((x >= 1.e6) || (x <= 1e-06))) {
        return se(x);
    }
    r = mk(Ct, 0);
    i = (int64_t)(x);
    if (i == 0ll) {
        r = cat1(r, Kc(48));
    }
    for (; (i != 0ll);) {
        r = cat1(r, Kc((int32_t)((48ll + (i % 10ll)))));
        i = (i / 10ll);
    }
    r = Rev(r);
    r = cat1(r, Kc(46));
    x = (x - F64floor(x));
    {
        i15 = (int32_t)(0);
        do {
            x = (x * 10.);
            r = cat1(r, Kc((48 + ((int32_t)(x) % 10))));
            i15++;
        } while (i15 < 6);
    }
    n = nn(r);
    rp = (int32_t)(r);
    for (; (n > 0);) {
        n = (n - 1);
        if (I8(rp) == 48) {
            c++;
        } else {
            c = 0;
        }
        rp++;
    }
    return ndrop(-c, r);
}

static uint64_t se(double x)
{
    int32_t ei;
    double f;
    int64_t e;
    f = x;
    e = (int64_t)(0ll);
    if (frexp1(x)) {
        f = frexp2(x);
        e = frexp3(x);
    }
    x = (0.3010299956639812 * (double)(e));
    ei = (int32_t)(F64floor(x));
    x = (x - (double)(ei));
    return ucat(cat1(sf((f * pow_(10., x))), Kc(101)), si(ei));
}

static uint64_t sfz(double re, double im)
{
    uint64_t r;
    double z, a;
    if ((re != re) || (im != im)) {
        return Ku(6385200ull);
    }
    z = hypot_(re, im);
    a = ang2(im, re);
    r = cat1(trdot(sf(z)), Kc(97));
    if (a != 0.) {
        r = ucat(r, trdot(sf(a)));
    }
    return r;
}

static uint64_t trdot(uint64_t x)
{
    int32_t n;
    n = nn(x);
    if (I8((((int32_t)(x) + n) - 1)) == 46) {
        return ndrop(-1, x);
    }
    return x;
}

static uint64_t Cst(uint64_t x, uint64_t y)
{
    int32_t yt, t;
    yt = tp(y);
    if (yt > Zt) {
        return Ecr(17ull, l2(x, y));
    }
    if (yt == ct) {
        y = Enl(y);
        yt = Ct;
    }
    if ((tp(x) != st) || (yt != Ct)) {
        trap();
    }
    if (!(int32_t)(x)) {
        return sc(y);
    }
    t = ts(x);
    y = val(y);
    yt = tp(y);
    if (t == yt) {
        return y;
    }
    if ((y == 0ull) && (t > 16)) {
        return mk(t, 0);
    }
    if ((t - yt) > 15) {
        y = Enl(y);
    }
    if ((t & 15) > (yt & 15)) {
        y = uptype(y, (t & 15));
    }
    return y;
}

static int32_t ts(uint64_t x)
{
    int32_t c;
    c = inC((int32_t)(Rdc(2ull, l1(cs(x)))), 254, 279);
    if (c > 0) {
        return (int32_t)((c - 253));
    }
    return 0;
}

static uint64_t rtp(int32_t t, uint64_t x)
{
    int32_t xt, n, m;
    xt = tp(x);
    if ((uint32_t)((xt - 18)) > 5) {
        trap();
    }
    n = (nn(x) * sz(xt));
    m = (n / sz(t));
    if (n != (m * sz(t))) {
        trap();
    }
    x = use(x);
    SetI32(((int32_t)(x)-12), m);
    return ti(t, (int32_t)(x));
}

void repl(uint64_t x)
{
    int32_t c;
    c = I8((int32_t)(x));
    x = val(x);
    if (x != 0ull) {
        if (c == 32) {
            dx(Out(x));
        } else {
            write(cat1(join(Kc(10), Lst(x)), Kc(10)));
        }
    }
}

static uint64_t Out(uint64_t x)
{
    write(cat1(Kst(rx(x)), Kc(10)));
    return x;
}

static uint64_t Otu(uint64_t x, uint64_t y)
{
    write(cat1(Kst(x), Kc(58)));
    return Out(y);
}

static void write(uint64_t x)
{
    Write(0, 0, (int32_t)(x), nn(x));
    dx(x);
}

static uint64_t readfile(uint64_t x)
{
    uint64_t r;
    int32_t n;
    if (!nn(x)) {
        dx(x);
        r = mk(Ct, 496);
        r = ntake(ReadIn((int32_t)(r), 496), r);
        return r;
    }
    n = Read((int32_t)(x), nn(x), 0);
    if (n < 0) {
        dx(x);
        return mk(Ct, 0);
    }
    r = mk(Ct, n);
    Read((int32_t)(x), nn(x), (int32_t)(r));
    dx(x);
    return r;
}

static uint64_t writefile(uint64_t x, uint64_t y)
{
    int32_t r;
    r = Write((int32_t)(x), nn(x), (int32_t)(y), nn(y));
    if (r) {
        trap();
    }
    dx(x);
    return y;
}

static void test(uint64_t x)
{
    uint64_t l;
    int32_t n, i3;
    if (tp(x) != Ct) {
        trap();
    }
    l = ndrop(-1, split(Kc(10), rx(x)));
    n = nn(l);
    dx(l);
    {
        i3 = (int32_t)(0);
        for (; (i3 < n); i3++)
            testi(rx(x), i3);
    }
    dx(x);
}

static void testi(uint64_t l, int32_t i)
{
    uint64_t x, y;
    x = split(Ku(12064ull), ati(split(Kc(10), l), i));
    if (nn(x) != 2) {
        trap();
    }
    y = x1(x);
    x = r0(x);
    dx(Out(ucat(ucat(rx(x), Ku(12064ull)), rx(y))));
    x = Kst(val(x));
    if (!match(x, y)) {
        x = Out(x);
        trap();
    }
    dxy(x, y);
}

static uint64_t tok(uint64_t x)
{
    uint64_t s, r, y6;
    int32_t i5;
    s = cat1(src(), Kc(10));
    pp = nn(s);
    s = Cat(s, x);
    pp += (int32_t)(s);
    pe = (pp + nn(x));
    r = mk(Lt, 0);
    for (;;) {
        ws();
        if (pp == pe) {
            break;
        }
        {
            i5 = (int32_t)(193);
            for (; (i5 < 200); i5++) {
                y6 = ((uint64_t (*)(void))F_[i5])();
                if (y6 != 0ull) {
                    y6 = (y6
                          | (uint64_t)(((int64_t)((pp - (int32_t)(s)))
                                        << 32ll)));
                    r = cat1(r, y6);
                    break;
                }
            }
        }
    }
    SetI32(16, (int32_t)(s));
    return r;
}

uint64_t src(void) { return ti(Ct, I32(16)); }

static uint64_t tchr(void)
{
    uint64_t r;
    int32_t c8;
    uint32_t q;
    if ((I8(pp) == 48) && (pp < pe)) {
        if (I8((1 + pp)) == 120) {
            pp += 2;
            return thex();
        }
    }
    if (I8(pp) != 34) {
        return 0ull;
    }
    pp++;
    r = mk(Ct, 0);
    q = (uint32_t)(0);
    for (;;) {
        if (pp == pe) {
            srcp = (pe - I32(16));
            trap();
        }
        c8 = I8(pp);
        pp++;
        if ((c8 == 34) && (q == 0)) {
            break;
        }
        if ((c8 == 92) && (q == 0)) {
            q = 1;
            continue;
        }
        if (q) {
            c8 = cq(c8);
            q = 0;
        }
        r = cat1(r, Kc(c8));
    }
    if (nn(r) == 1) {
        return Fst(r);
    }
    return r;
}

static int32_t cq(int32_t c)
{
    if (c == 116) {
        return 9;
    }
    if (c == 110) {
        return 10;
    }
    if (c == 114) {
        return 13;
    }
    return c;
}

static uint64_t thex(void)
{
    uint64_t r;
    int32_t c2;
    r = mk(Ct, 0);
    for (; (pp < (pe - 1));) {
        c2 = I8(pp);
        if (!is(c2, 128)) {
            break;
        }
        r = cat1(r, Kc(((hx(c2) << 4) + hx(I8((1 + pp))))));
        pp += 2;
    }
    if (nn(r) == 1) {
        return Fst(r);
    }
    return r;
}

static int32_t hx(int32_t c) { return (is(c, 4)) ? ((c - 48)) : ((c - 87)); }

static uint64_t tnms(void)
{
    uint64_t r, x2;
    int32_t t2;
    r = tnum();
    for (; ((pp < (pe - 1)) && (I8(pp) == 32));) {
        pp++;
        x2 = tnum();
        if (x2 == 0ull) {
            break;
        }
        t2 = tp(r);
        if (t2 < 16) {
            r = Enl(r);
        }
        t2 = maxtype(r, x2);
        r = uptype(r, t2);
        r = cat1(r, uptype(x2, t2));
    }
    return r;
}

static uint64_t tnum(void)
{
    uint64_t r6;
    int32_t c;
    c = I8(pp);
    if ((c == 45) || (c == 46)) {
        if (is(I8((pp - 1)), 64)) {
            return 0ull;
        }
    }
    if ((c == 45) && (pp < (1 + pe))) {
        pp++;
        r6 = tunm();
        if (r6 == 0ull) {
            pp = (pp - 1);
            return 0ull;
        }
        return Neg(r6);
    }
    return tunm();
}

static uint64_t tunm(void)
{
    uint64_t q20;
    int32_t p, c8;
    int64_t r;
    p = pp;
    r = pu();
    if ((r == 0ll) && (p == pp)) {
        if (I8(p) == 46) {
            if (is(I8((1 + p)), 4)) {
                return pflt(r);
            }
        }
        return 0ull;
    }
    if (pp < pe) {
        c8 = I8(pp);
        if (c8 == 46) {
            return pflt(r);
        }
        if (c8 == 112) {
            return ppi((double)(r));
        }
        if (c8 == 97) {
            return pflz((double)(r));
        }
        if ((c8 == 101) || (c8 == 69)) {
            return Kf(pexp((double)(r)));
        }
        if (r == 0ll) {
            if ((c8 == 110) || (c8 == 119)) {
                q20 = Kf(0.);
                SetI64((int32_t)(q20), (int64_t)(0x7ff8000000000001ll));
                if (c8 == 119) {
                    SetF64((int32_t)(q20), inf);
                }
                pp++;
                if ((pp < pe) && (I8(pp) == 97)) {
                    dx(q20);
                    return pflz(F64((int32_t)(q20)));
                }
                return q20;
            }
        }
        if (is(c8, 2)) {
            srcp = ((1 + pp) - I32(16));
            trap();
        }
    }
    return Ki((int32_t)(r));
}

static int64_t pu(void)
{
    int32_t c2;
    int64_t r;
    r = (int64_t)(0ll);
    for (; (pp < pe);) {
        c2 = I8(pp);
        if (!is(c2, 4)) {
            break;
        }
        r = ((10ll * r) + (int64_t)((c2 - 48)));
        pp++;
    }
    return r;
}

static double pexp(double f)
{
    int32_t c2;
    int64_t e;
    pp++;
    e = (int64_t)(1ll);
    if (pp < pe) {
        c2 = I8(pp);
        if ((c2 == 45) || (c2 == 43)) {
            if (c2 == 45) {
                e = (int64_t)(-1ll);
            }
            pp++;
        }
    }
    e = (e * pu());
    return (f * pow_(10., (double)(e)));
}

static uint64_t pflt(int64_t i)
{
    int32_t c;
    double d, f;
    d = 1.;
    f = (double)(i);
    pp++;
    for (; (pp < pe);) {
        c = I8(pp);
        if (!is(c, 4)) {
            break;
        }
        d = (d / 10.);
        f += (d * (double)((c - 48)));
        pp++;
    }
    if (pp < pe) {
        c = I8(pp);
        if ((c == 101) || (c == 69)) {
            f = pexp(f);
        }
    }
    if (pp < pe) {
        c = I8(pp);
        if (c == 97) {
            return pflz(f);
        }
        if (c == 112) {
            return ppi(f);
        }
    }
    return Kf(f);
}

static uint64_t pflz(double f)
{
    uint64_t r;
    r = (uint64_t)(0ull);
    pp++;
    if (pp < pe) {
        r = tunm();
    }
    return Rot(Kf(f), r);
}

static uint64_t ppi(double f)
{
    pp++;
    return Kf((pi * f));
}

static uint64_t tvrb(void)
{
    int32_t c, o;
    c = I8(pp);
    if (!is(c, 1)) {
        return 0ull;
    }
    pp++;
    if ((c == 92) && (I8((pp - 2)) == 32)) {
        return (uint64_t)(29ull);
    }
    o = (int32_t)(1);
    if (pp < pe) {
        if (I8(pp) == 58) {
            pp++;
            if (is(c, 8)) {
                srcp = (pp - I32(16));
                trap();
            }
            o = 97;
        }
    }
    return (uint64_t)((o + idx(c, 227, 253)));
}

static uint64_t tpct(void)
{
    int32_t c;
    c = I8(pp);
    if (is(c, 48)) {
        pp++;
        return (uint64_t)(c);
    }
    if (c == 10) {
        pp++;
        return (uint64_t)(59ull);
    }
    return 0ull;
}

static uint64_t tvar(void)
{
    uint64_t r;
    int32_t c;
    c = I8(pp);
    if (!is(c, 2)) {
        return 0ull;
    }
    pp++;
    r = Ku((uint64_t)(c));
    for (; (pp < pe);) {
        c = I8(pp);
        if (!is(c, 6)) {
            break;
        }
        r = cat1(r, ti(ct, c));
        pp++;
    }
    return sc(r);
}

static uint64_t tsym(void)
{
    uint64_t r, s2;
    r = (uint64_t)(0ull);
    for (; (I8(pp) == 96);) {
        pp++;
        if (r == 0ull) {
            r = mk(St, 0);
        }
        s2 = (uint64_t)(0ull);
        if (pp < pe) {
            s2 = tchr();
            if (tp(s2) == ct) {
                s2 = sc(Enl(s2));
            } else {
                s2 = (s2 != 0ull) ? (sc(s2)) : (tvar());
            }
        }
        if (s2 == 0ull) {
            s2 = ((uint64_t)(st) << 59ull);
        }
        r = cat1(r, s2);
        if (pp == pe) {
            break;
        }
    }
    return r;
}

static void ws(void)
{
    int32_t c;
    for (; (pp < pe);) {
        c = I8(pp);
        if ((c == 10) || (c > 32)) {
            break;
        }
        pp++;
    }
    for (; (pp < pe);) {
        c = I8(pp);
        if ((c == 47) && (I8((pp - 1)) < 33)) {
            pp++;
            for (; (pp < pe);) {
                c = I8(pp);
                if (c == 10) {
                    break;
                }
                pp++;
            }
        } else {
            return;
        }
    }
}

static int32_t is(int32_t x, int32_t m) { return (m & I8((100 + x))); }

static uint64_t nyi(uint64_t x)
{
    trap();
    return 0ull;
}

static uint64_t Idy(uint64_t x) { return x; }

static uint64_t Dex(uint64_t x, uint64_t y)
{
    dx(x);
    return y;
}

static uint64_t Flp(uint64_t x)
{
    uint64_t m2, r4;
    int32_t xt, n2, xp2, m7;
    xt = tp(x);
    if (xt == Lt) {
        n2 = nn(x);
        xp2 = (int32_t)(x);
        m2 = Ki(maxcount(xp2, n2));
        x = Atx(Rdc(13ull, l1(Ecr(15ull, l2(m2, x)))),
                Ecl(2ull, l2(Til(m2), Mul(m2, Til(Ki(n2))))));
    } else {
        if (xt > Lt) {
            r4 = x0(x);
            x = r1(x);
            x = (xt == Tt) ? (Key(r4, x)) : ({
                if ((tp(r4) != St) || (tp(x) != Lt)) {
                    trap();
                }
                m7 = maxcount((int32_t)(x), nn(x));
                x = Ech(15ull, l2(Ki(m7), x));
                r4 = l2(r4, x);
                SetI32(((int32_t)(r4)-12), m7);
                ti(Tt, (int32_t)(r4));
            });
        }
    }
    return x;
}

static int32_t maxcount(int32_t xp, int32_t n)
{
    uint64_t x2;
    int32_t r;
    r = (int32_t)(0);
    for (; (n > 0);) {
        n = (n - 1);
        x2 = (uint64_t)(I64(xp));
        xp += 8;
        r = (tp(x2) < 16) ? (maxi(1, r)) : (maxi(nn(x2), r));
    }
    return r;
}

static uint64_t Fst(uint64_t x)
{
    int32_t t;
    t = tp(x);
    if (t < 16) {
        return x;
    }
    if (t == Dt) {
        return Fst(Val(x));
    }
    return ati(x, 0);
}

static uint64_t Las(uint64_t x)
{
    int32_t t, n;
    t = tp(x);
    if (t < 16) {
        return x;
    }
    if (t == Dt) {
        x = Val(x);
    }
    n = nn(x);
    if (!n) {
        return Fst(x);
    }
    return ati(x, (n - 1));
}

static uint64_t Cnt(uint64_t x)
{
    int32_t t;
    t = tp(x);
    dx(x);
    if (t < 16) {
        return Ki(1);
    }
    return Ki(nn(x));
}

static uint64_t Not(uint64_t x)
{
    x = ((tp(x) & 15) == st) ? (Eql(Ks(0), x)) : (Eql(Ki(0), x));
    return x;
}

static uint64_t Til(uint64_t x)
{
    uint64_t t2;
    int32_t xt;
    xt = tp(x);
    if (xt > Lt) {
        t2 = x0(x);
        dx(x);
        return t2;
    }
    if (xt == it) {
        return seq((int32_t)(x));
    }
    if (xt == It) {
        return Enc(x, Til(Rdc(4ull, l1(rx(x)))));
    }
    trap();
    return 0ull;
}

static uint64_t Unq(uint64_t x)
{
    uint64_t r;
    int32_t xt;
    xt = tp(x);
    if (xt < 16) {
        return roll(x);
    }
    if (xt >= Lt) {
        if (xt == Dt) {
            trap();
        }
        if (xt == Tt) {
            r = x0(x);
            x = r1(x);
            return key(r, Flp(Unq(Flp(x))), xt);
        }
    }
    rx(rx(x));
    return atv(x, Wer(Eql(seq(nn(x)), Fnd(x, x))));
}

static uint64_t Grp(uint64_t x)
{
    if (tp(x) == it) {
        return mat(10ull, x);
    }
    return kx(96, x);
}

static uint64_t mat(uint64_t f, uint64_t x)
{
    x = rx(seq((int32_t)(x)));
    return Ecr(f, l2(x, x));
}

static uint64_t Key(uint64_t x, uint64_t y) { return key(x, y, Dt); }

static uint64_t key(uint64_t x, uint64_t y, int32_t t)
{
    int32_t xt, yt, xn;
    xt = tp(x);
    yt = tp(y);
    if (xt < 16) {
        if (xt == it) {
            return Mod(y, x);
        }
        if (xt == st) {
            if (yt == Tt) {
                x = rx(x);
                y = rx(y);
                return Key(Tak(x, y), Drp(x, y));
            }
        }
        x = Enl(x);
    }
    xn = nn(x);
    if (t == Tt) {
        if (xn > 0) {
            xn = nn((uint64_t)(I64((int32_t)(y))));
        }
    } else {
        if (yt < 16) {
            trap();
        } else {
            if (xn != nn(y)) {
                trap();
            }
        }
    }
    x = l2(x, y);
    SetI32(((int32_t)(x)-12), xn);
    return ti(t, (int32_t)(x));
}

static uint64_t Tak(uint64_t x, uint64_t y)
{
    uint64_t r4;
    int32_t xt, yt;
    xt = tp(x);
    yt = tp(y);
    if (yt == Dt) {
        x = rx(x);
        return (xt == it) ? ({
            r4 = x0(y);
            y = r1(y);
            r4 = Tak(x, r4);
            y = Tak(x, y);
            Key(r4, y);
        })
                          : (Key(x, Atx(y, x)));
    } else {
        if (yt == Tt) {
            return ((xt & 15) == st) ? ({
                if (xt == st) {
                    x = Enl(x);
                }
                x = rx(x);
                key(x, Atx(y, x), yt);
            })
                                     : (Ecr(15ull, l2(x, y)));
        }
    }
    if (xt == it) {
        return ntake((int32_t)(x), y);
    }
    y = rx(y);
    if ((xt > 16) && (xt == yt)) {
        return atv(y, Wer(In(y, x)));
    }
    return Atx(y, Wer(Cal(x, l1(y))));
}

static uint64_t ntake(int32_t n, uint64_t y)
{
    uint64_t r;
    int32_t t, yn, s, yp;
    t = tp(y);
    if (n < 0) {
        if (tp(y) < 16) {
            return ntake(-n, y);
        }
        n += nn(y);
        if (n < 0) {
            return ucat(ntake(-n, missing((t - 16))), y);
        }
        return ndrop(n, y);
    }
    if (t < 16) {
        return atv(Enl(y), Wer(Ki(n)));
    }
    yn = nn(y);
    s = sz(t);
    yp = (int32_t)(y);
    if ((((I32((yp - 4)) == 1) && (bucket((s * yn)) == bucket((s * n))))
         && (n <= yn))
        && (t < Lt)) {
        SetI32((yp - 12), n);
        return y;
    }
    r = seq(n);
    if ((n > yn) && (yn > 0)) {
        r = Mod(r, Ki(yn));
    }
    return atv(y, r);
}

static uint64_t Drp(uint64_t x, uint64_t y)
{
    uint64_t r4;
    int32_t xt, yt;
    xt = tp(x);
    yt = tp(y);
    if (yt > Lt) {
        return ((yt == Dt) || ((yt == Tt) && ((xt & 15) == st)))
            ? ({
                  r4 = x0(y);
                  y = r1(y);
                  if (xt < 16) {
                      x = Enl(x);
                  }
                  x = rx(Wer(Not(In(rx(r4), x))));
                  key(Atx(r4, x), Atx(y, x), yt);
              })
            : (Ecr(16ull, l2(x, y)));
    }
    if (xt == it) {
        return ndrop((int32_t)(x), y);
    }
    if ((xt > 16) && (xt == yt)) {
        return atv(y, Wer(Not(In(rx(y), x))));
    }
    if (yt == it) {
        return atv(x, Wer(Not(Eql(y, seq(nn(x))))));
    }
    return Atx(y, Wer(Not(Cal(x, l1(rx(y))))));
}

static uint64_t ndrop(int32_t n, uint64_t y)
{
    uint64_t r;
    int32_t yt, yn, rn, s, yp, rp;
    yt = tp(y);
    if ((yt < 16) || (yt > Lt)) {
        trap();
    }
    yn = nn(y);
    if (n < 0) {
        return ntake(maxi(0, (yn + n)), y);
    }
    rn = (yn - n);
    if (rn < 0) {
        dx(y);
        return mk(yt, 0);
    }
    s = sz(yt);
    yp = (int32_t)(y);
    if (((I32((yp - 4)) == 1) && (bucket((s * yn)) == bucket((s * rn))))
        && (yt < Lt)) {
        r = rx(y);
        SetI32((yp - 12), rn);
    } else {
        r = mk(yt, rn);
    }
    rp = (int32_t)(r);
    Memorycopy(rp, (yp + (s * n)), (s * rn));
    if (yt == Lt) {
        rl(r);
        r = uf(r);
    }
    dx(y);
    return r;
}

static uint64_t Cut(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t yt, xt, xp, rp, e, n;
    yt = tp(y);
    if ((yt == it) || (yt == ft)) {
        return Pow(y, x);
    }
    xt = tp(x);
    if (xt == It) {
        return cuts(x, y);
    }
    if ((xt == Ct) && (yt == Ct)) {
        x = rx(Wer(In(rx(y), x)));
        return rcut(y, Cat(Ki(0), Add(Ki(1), x)), Cat(x, Ki(nn(y))));
    }
    if ((xt != it) || (yt < 16)) {
        trap();
    }
    xp = (int32_t)(x);
    if (xp <= 0) {
        xp = (nn(y) / -xp);
    }
    r = mk(Lt, xp);
    rp = (int32_t)(r);
    e = ep(r);
    n = (nn(y) / xp);
    x = seq(n);
    do {
        SetI64(rp, (int64_t)(atv(rx(y), rx(x))));
        x = Add(Ki(n), x);
        rp += 8;
    } while (rp < e);
    dxy(x, y);
    return r;
}

static uint64_t cuts(uint64_t x, uint64_t y)
{
    return rcut(y, x, cat1(ndrop(1, rx(x)), Ki(nn(y))));
}

static uint64_t rcut(uint64_t x, uint64_t a, uint64_t b)
{
    uint64_t r;
    int32_t n, ap, bp, rp, o2, m2;
    n = nn(a);
    {
        ap = (int32_t)(a);
        bp = (int32_t)(b);
    }
    r = mk(Lt, n);
    rp = (int32_t)(r);
    for (; (n > 0);) {
        n = (n - 1);
        o2 = I32(ap);
        m2 = (I32(bp) - o2);
        if (m2 < 0) {
            trap();
        }
        SetI64(rp, (int64_t)(atv(rx(x), Add(Ki(o2), seq(m2)))));
        rp += 8;
        ap += 4;
        bp += 4;
    }
    dxy(a, b);
    dx(x);
    return r;
}

static uint64_t split(uint64_t x, uint64_t y)
{
    int32_t xt, yt, xn;
    {
        xt = tp(x);
        yt = tp(y);
    }
    xn = (int32_t)(1);
    if (yt == (xt + 16)) {
        x = Wer(Eql(x, rx(y)));
    } else {
        if ((xt == yt) && (xt == Ct)) {
            xn = nn(x);
            x = Find(x, rx(y));
        } else {
            trap();
        }
    }
    x = rx(x);
    return rcut(y, Cat(Ki(0), Add(Ki(xn), x)), cat1(x, Ki(nn(y))));
}

static uint64_t join(uint64_t x, uint64_t y)
{
    int32_t n;
    n = -(int32_t)(Cnt(rx(x)));
    return ndrop(n, Rdc(13ull, l1(Ecl(13ull, l2(y, x)))));
}

static uint64_t Bin(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t xt, yt;
    xt = tp(x);
    yt = tp(y);
    if ((xt == yt) || (yt == Lt)) {
        return Ecr(40ull, l2(x, y));
    } else {
        if (xt == (yt + 16)) {
            r = Ki(ibin(x, y, xt));
        } else {
            trap();
        }
    }
    dxy(x, y);
    return r;
}

static int32_t ibin(uint64_t x, uint64_t y, int32_t t)
{
    int32_t h, k, n, xp, yp, j, s;
    double f18;
    k = (int32_t)(0);
    n = nn(x);
    xp = (int32_t)(x);
    yp = (int32_t)(y);
    j = (n - 1);
    s = sz(t);
    switch ((s >> 2)) {
    case 0:
        for (;;) {
            if (k > j) {
                return (k - 1);
            }
            h = ((k + j) >> 1);
            if (I8((xp + h)) > yp) {
                j = (h - 1);
            } else {
                k = (h + 1);
            }
        }
        break;
    case 1:
        for (;;) {
            if (k > j) {
                return (k - 1);
            }
            h = ((k + j) >> 1);
            if (I32((xp + (4 * h))) > yp) {
                j = (h - 1);
            } else {
                k = (h + 1);
            }
        }
        break;
    default: {
        f18 = F64(yp);
        for (;;) {
            if (k > j) {
                return (k - 1);
            }
            h = ((k + j) >> 1);
            if (F64((xp + (8 * h))) > f18) {
                j = (h - 1);
            } else {
                k = (h + 1);
            }
        }
    } break;
    }
    return 0;
}

static uint64_t Flr(uint64_t x)
{
    uint64_t r;
    int32_t rp, xt, xp, xn, e12;
    rp = (int32_t)(0);
    xt = tp(x);
    xp = (int32_t)(x);
    if (xt < 16) {
        switch ((xt - 2)) {
        case 0:
            return Kc(lc(xp));
            break;
        case 1:
            return Kc(xp);
            break;
        case 2:
            return Ki((int32_t)(xp));
            break;
        case 3: {
            dx(x);
            return Ki((int32_t)(F64floor(F64(xp))));
        } break;
        case 4: {
            dx(x);
            return Kf(F64(xp));
        } break;
        default:
            return x;
            break;
        }
    }
    xn = nn(x);
    switch ((xt - 18)) {
    case 0:
        return lower(x);
        break;
    case 1: {
        r = mk(Ct, xn);
        rp = (int32_t)(r);
        e12 = (rp + xn);
        for (; (rp < e12);) {
            SetI8(rp, I32(xp));
            xp += 4;
            rp++;
        }
    } break;
    case 2: {
        x = use(x);
        return ti(It, (int32_t)(x));
    } break;
    case 3: {
        r = mk(It, xn);
        rp = (int32_t)(r);
        for (; (xn > 0);) {
            xn = (xn - 1);
            SetI32(rp, (int32_t)(F64floor(F64(xp))));
            xp += 8;
            rp += 4;
        }
    } break;
    case 4:
        return reim(x, 0);
        break;
    default:
        return Ech(16ull, l1(x));
        break;
    }
    dx(x);
    return r;
}

static uint64_t reim(uint64_t x, int32_t o)
{
    uint64_t r;
    int32_t p3, e3;
    if (tp(x) < 16) {
        r = Kf(F64(((int32_t)(x) + o)));
    } else {
        r = mk(Ft, nn(x));
        p3 = (int32_t)(r);
        o += (int32_t)(x);
        e3 = ep(r);
        for (; (p3 < e3);) {
            SetF64(p3, F64(o));
            o += 16;
            p3 += 8;
        }
    }
    dx(x);
    return r;
}

static uint64_t lower(uint64_t x)
{
    int32_t p, e;
    x = use(x);
    p = (int32_t)(x);
    e = (p + nn(x));
    for (; (p < e);) {
        SetI8(p, lc(I8(p)));
        p++;
    }
    return x;
}

static int32_t lc(int32_t x)
{
    return (x + (32 * I32B(((uint32_t)((x - 65)) < 26))));
}

static uint64_t Rev(uint64_t x)
{
    uint64_t r;
    int32_t t, xn, rp;
    t = tp(x);
    if (t < 16) {
        if (t == it) {
            return Rev(Grp(x));
        }
        trap();
    }
    if (t == Dt) {
        r = x0(x);
        return Key(Rev(r), Rev(r1(x)));
    }
    xn = nn(x);
    if (xn < 2) {
        return x;
    }
    r = mk(It, xn);
    rp = (int32_t)(r);
    for (; (xn > 0);) {
        xn = (xn - 1);
        SetI32(rp, xn);
        rp += 4;
    }
    return atv(x, r);
}

static uint64_t Wer(uint64_t x)
{
    uint64_t r;
    int32_t t, xn, xp, n6, rp6, i7, j8;
    r = (uint64_t)(0ull);
    t = tp(x);
    if (t < 16) {
        x = Enl(x);
        t = tp(x);
    }
    if (t == Dt) {
        r = x0(x);
        return Atx(r, Wer(r1(x)));
    }
    xn = nn(x);
    xp = (int32_t)(x);
    if (t == It) {
        n6 = sumi(xp, ep(x));
        r = mk(It, n6);
        rp6 = (int32_t)(r);
        {
            i7 = (int32_t)(0);
            for (; (i7 < xn); i7++) {
                j8 = I32(xp);
                for (; (j8 > 0);) {
                    j8 = (j8 - 1);
                    SetI32(rp6, i7);
                    rp6 += 4;
                }
                xp += 4;
            }
        }
    } else {
        if (!xn) {
            r = mk(It, 0);
        } else {
            trap();
        }
    }
    dx(x);
    return r;
}

static uint64_t Fwh(uint64_t x)
{
    int32_t t, p2, e2;
    t = tp(x);
    if (t == It) {
        dx(x);
        p2 = (int32_t)(x);
        e2 = ep(x);
        for (; (p2 < e2);) {
            if (I32(p2)) {
                return Ki(((p2 - (int32_t)(x)) >> 2));
            }
            p2 += 4;
        }
        return Ki(0);
    }
    return Fst(Wer(x));
}

static uint64_t Typ(uint64_t x)
{
    dx(x);
    return sc(Ku((uint64_t)(I8((253 + (int32_t)(tp(x)))))));
}

static uint64_t Tok(uint64_t x) { return (tp(x) == Ct) ? (tok(x)) : (x); }

uint64_t Val(uint64_t x)
{
    uint64_t r8;
    int32_t xt;
    xt = tp(x);
    if (xt == st) {
        return lup(x);
    }
    if (xt == Ct) {
        return val(x);
    }
    if ((xt & 15) == zt) {
        rx(x);
        return ucat(Enl(reim(x, 0)), Enl(reim(x, 8)));
    }
    if ((xt == lf) || (xt == xf)) {
        r8 = l2(x0(x), x1(x));
        if (xt == lf) {
            r8 = cat1(r8, x2(x));
        }
        dx(x);
        return cat1(r8, Ki(nn(x)));
    }
    if (xt == Lt) {
        return exec(x);
    }
    return (xt > Lt) ? (r1(x)) : ({
        trap();
        0ull;
    });
}

static uint64_t val(uint64_t x)
{
    int32_t xn, xp, a;
    x = parse(tok(x));
    xn = nn(x);
    xp = ((int32_t)(x) + (8 * (xn - 1)));
    a = (int32_t)(0);
    if ((xn > 2) && (I64(xp) == 64ll)) {
        a = 1;
    }
    x = exec(x);
    if (a) {
        dx(x);
        return 0ull;
    }
    return x;
}

static uint64_t Enc(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t yt, yi, n, xi8;
    yt = tp(y);
    if (yt == It) {
        return cal(lup(Ks(104)), l2(x, y));
    }
    if (yt != it) {
        trap();
    }
    yi = (int32_t)(y);
    n = (int32_t)(0);
    if (tp(x) == It) {
        n = nn(x);
    }
    r = mk(It, 0);
    for (;;) {
        n = (n - 1);
        xi8 = (int32_t)(ati(rx(x), n));
        r = cat1(r, Ki(modi(yi, xi8)));
        yi = divi(yi, xi8);
        if (!n) {
            break;
        }
        if ((n < 0) && ((uint32_t)((yi + 1)) < 2)) {
            if (yi == -1) {
                r = cat1(r, Ki(-1));
            }
            break;
        }
    }
    dx(x);
    return Rev(r);
}

static uint64_t Dec(uint64_t x, uint64_t y)
{
    uint64_t r;
    int32_t n, i3;
    if (tp(y) < 16) {
        trap();
    }
    r = Fst(rx(y));
    n = nn(y);
    {
        i3 = (int32_t)(1);
        for (; (i3 < n); i3++)
            r = Add(ati(rx(y), i3), Mul(ati(rx(x), i3), r));
    }
    dxy(x, y);
    return r;
}

static void zk(void)
{
    uint64_t x;
    int32_t zn;
    zn = (int32_t)(1348);
    x = mk(Ct, zn);
    Memorycopy((int32_t)(x), 280, zn);
    dx(Val(x));
}

void init()
{
    Memory(1);
    Memory2(1);
    memcpy(
        M_,
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x40\x01"
        "\x01\x01\x01\x09\x10\x60\x01\x01\x01\x01\x01\x09\xc4\xc4\xc4\xc4\xc4"
        "\xc4\xc4\xc4\xc4\xc4\x01\x20\x01\x01\x01\x01\x01\x42\x42\x42\x42\x42"
        "\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"
        "\x42\x42\x42\x42\x10\x09\x60\x01\x01\x00\xc2\xc2\xc2\xc2\xc2\xc2\x42"
        "\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42\x42"
        "\x42\x42\x10\x01\x60\x01\x3a\x2b\x2d\x2a\x25\x26\x7c\x3c\x3e\x3d\x7e"
        "\x21\x2c\x5e\x23\x5f\x24\x3f\x40\x2e\x27\x3a\x2f\x3a\x5c\x3a\x76\x62"
        "\x63\x69\x73\x66\x7a\x6c\x64\x74\x6d\x64\x70\x6c\x78\x30\x30\x42\x43"
        "\x49\x53\x46\x5a\x4c\x44\x54\x30\x60\x6b\x60\x6c\x60\x61\x60\x62\x60"
        "\x77\x68\x69\x6c\x65\x60\x22\x72\x66\x2e\x22\x60\x22\x72\x7a\x2e\x22"
        "\x60\x22\x67\x64\x74\x2e\x22\x60\x22\x67\x72\x70\x2e\x22\x60\x22\x65"
        "\x6e\x63\x2e\x22\x0a\x60\x78\x3a\x2c\x2f\x2b\x22\x30\x31\x32\x33\x34"
        "\x35\x36\x37\x38\x39\x61\x62\x63\x64\x65\x66\x22\x40\x31\x36\x20\x31"
        "\x36\x5c\x32\x35\x36\x21\x0a\x60\x74\x3a\x60\x33\x39\x0a\x60\x70\x3a"
        "\x60\x34\x36\x0a\x60\x65\x6e\x63\x3a\x7b\x24\x5b\x23\x79\x3b\x2b\x28"
        "\x26\x27\x2c\x27\x28\x7c\x2f\x63\x29\x2d\x63\x3a\x23\x27\x72\x29\x2c"
        "\x27\x72\x3a\x7b\x78\x5c\x79\x7d\x2f\x5b\x78\x3b\x79\x5d\x3b\x28\x23"
        "\x78\x29\x23\x2c\x21\x30\x5d\x7d\x0a\x60\x67\x64\x74\x3a\x7b\x5b\x74"
        "\x3b\x67\x5d\x28\x24\x5b\x67\x3b\x7b\x78\x40\x3e\x79\x20\x78\x7d\x3b"
        "\x7b\x78\x40\x3c\x79\x20\x78\x7d\x5d\x29\x2f\x28\x2c\x21\x23\x74\x29"
        "\x2c\x7c\x2e\x74\x7d\x0a\x60\x67\x72\x70\x3a\x7b\x28\x78\x40\x2a\x27"
        "\x67\x29\x21\x67\x3a\x28\x26\x7e\x61\x7e\x27\x61\x40\x2d\x31\x2b\x21"
        "\x23\x61\x3a\x78\x20\x69\x29\x5e\x69\x3a\x3c\x78\x7d\x0a\x61\x62\x73"
        "\x3a\x60\x33\x32\x3b\x73\x69\x6e\x3a\x60\x34\x34\x3b\x63\x6f\x73\x3a"
        "\x60\x34\x35\x3b\x66\x69\x6e\x64\x3a\x60\x33\x31\x3b\x69\x6d\x61\x67"
        "\x3a\x60\x33\x33\x3b\x63\x6f\x6e\x6a\x3a\x60\x33\x34\x3b\x61\x6e\x67"
        "\x6c\x65\x3a\x60\x33\x35\x3b\x65\x78\x70\x3a\x60\x34\x32\x3b\x6c\x6f"
        "\x67\x3a\x60\x34\x33\x0a\x60\x70\x61\x64\x3a\x7b\x78\x40\x5c\x21\x7c"
        "\x2f\x23\x27\x78\x7d\x0a\x60\x6c\x78\x79\x3a\x7b\x0a\x6b\x74\x3a\x7b"
        "\x5b\x78\x3b\x79\x3b\x6b\x3b\x54\x5d\x78\x3a\x24\x5b\x60\x54\x7e\x40"
        "\x78\x3b\x54\x5b\x78\x3b\x6b\x5d\x3b\x60\x70\x61\x64\x28\x22\x22\x3b"
        "\x22\x2d\x22\x29\x2c\x24\x78\x5d\x3b\x28\x78\x2c\x27\x22\x7c\x22\x29"
        "\x2c\x27\x54\x5b\x79\x3b\x6b\x5d\x7d\x0a\x64\x3a\x7b\x5b\x78\x3b\x6b"
        "\x3b\x6b\x74\x3b\x54\x5d\x72\x3a\x21\x78\x3b\x78\x3a\x2e\x78\x3b\x24"
        "\x5b\x60\x54\x7e\x40\x78\x3b\x6b\x74\x5b\x72\x3b\x78\x3b\x6b\x3b\x54"
        "\x5d\x3b\x2c\x27\x5b\x2c\x27\x5b\x60\x70\x61\x64\x28\x6b\x27\x72\x29"
        "\x3b\x22\x7c\x22\x5d\x3b\x6b\x27\x78\x5d\x5d\x7d\x0a\x54\x3a\x7b\x5b"
        "\x78\x3b\x6b\x5d\x24\x5b\x60\x4c\x3f\x40\x27\x2e\x78\x3b\x2c\x6b\x20"
        "\x78\x3b\x28\x2c\x2a\x78\x29\x2c\x28\x2c\x28\x23\x2a\x78\x29\x23\x22"
        "\x2d\x22\x29\x2c\x31\x5f\x78\x3a\x22\x20\x22\x2f\x27\x2b\x60\x70\x61"
        "\x64\x40\x27\x24\x28\x21\x78\x29\x2c\x27\x2e\x78\x5d\x7d\x0a\x74\x3a"
        "\x40\x79\x3b\x6b\x3a\x60\x6b\x78\x79\x40\x2a\x78\x3b\x68\x3a\x2a\x7c"
        "\x78\x0a\x64\x64\x3a\x28\x22\x22\x3b\x2c\x22\x2e\x2e\x22\x29\x68\x3c"
        "\x23\x79\x3a\x24\x5b\x28\x40\x79\x29\x3f\x60\x4c\x60\x44\x60\x54\x3b"
        "\x79\x3b\x79\x7e\x2a\x79\x3b\x79\x3b\x5b\x74\x3a\x60\x4c\x3b\x2c\x79"
        "\x5d\x5d\x0a\x79\x3a\x24\x5b\x79\x7e\x2a\x79\x3b\x79\x3b\x28\x68\x26"
        "\x23\x79\x29\x23\x79\x5d\x0a\x24\x5b\x60\x44\x7e\x74\x3b\x64\x5b\x79"
        "\x3b\x6b\x3b\x6b\x74\x3b\x54\x5d\x3b\x60\x54\x7e\x74\x3b\x54\x5b\x79"
        "\x3b\x6b\x5d\x3b\x79\x7e\x2a\x79\x3b\x2c\x6b\x20\x79\x3b\x6b\x27\x79"
        "\x5d\x2c\x64\x64\x7d\x0a\x60\x6c\x3a\x60\x6c\x78\x79\x20\x37\x30\x20"
        "\x32\x30\x0a\x60\x73\x74\x72\x3a\x7b\x71\x3a\x7b\x63\x2c\x28\x22\x5c"
        "\x5c\x22\x2f\x28\x30\x2c\x69\x29\x5e\x40\x5b\x78\x3b\x69\x3b\x28\x71"
        "\x73\x21\x22\x74\x6e\x72\x5c\x22\x5c\x5c\x22\x29\x78\x20\x69\x3a\x26"
        "\x78\x3f\x5c\x71\x73\x3a\x22\x5c\x74\x5c\x6e\x5c\x72\x5c\x22\x5c\x5c"
        "\x22\x5d\x29\x2c\x63\x3a\x5f\x33\x34\x7d\x0a\x24\x5b\x7c\x2f\x78\x3f"
        "\x5c\x22\x5c\x74\x5c\x6e\x5c\x72\x22\x5f\x5f\x21\x33\x31\x3b\x22\x30"
        "\x78\x22\x2c\x60\x78\x40\x78\x3b\x71\x20\x78\x5d\x7d\x0a\x60\x6b\x78"
        "\x79\x3a\x7b\x0a\x61\x3a\x7b\x74\x3a\x40\x78\x3b\x78\x3a\x24\x78\x3b"
        "\x24\x5b\x60\x63\x7e\x74\x3b\x60\x73\x74\x72\x20\x78\x3b\x60\x73\x7e"
        "\x74\x3b\x22\x60\x22\x2c\x78\x3b\x78\x5d\x7d\x0a\x64\x3a\x7b\x5b\x78"
        "\x3b\x6b\x5d\x72\x3a\x22\x21\x22\x2c\x6b\x40\x2e\x78\x3b\x6e\x3a\x23"
        "\x21\x78\x3b\x78\x3a\x6b\x40\x21\x78\x3b\x24\x5b\x28\x6e\x3c\x32\x29"
        "\x7c\x28\x40\x2e\x78\x29\x3f\x60\x44\x60\x54\x3b\x22\x28\x22\x2c\x78"
        "\x2c\x22\x29\x22\x3b\x78\x5d\x2c\x72\x7d\x0a\x76\x3a\x7b\x5b\x78\x3b"
        "\x6b\x3b\x6d\x5d\x74\x3a\x40\x78\x3b\x78\x3a\x28\x6d\x26\x6e\x3a\x23"
        "\x78\x29\x23\x78\x0a\x78\x3a\x24\x5b\x60\x4c\x7e\x74\x3b\x6b\x27\x78"
        "\x3b\x60\x43\x7e\x74\x3b\x78\x3b\x24\x78\x5d\x0a\x78\x3a\x24\x5b\x60"
        "\x43\x7e\x74\x3b\x60\x73\x74\x72\x20\x78\x3b\x60\x53\x7e\x74\x3b\x63"
        "\x2c\x28\x63\x3a\x22\x60\x22\x29\x2f\x78\x3b\x60\x4c\x7e\x74\x3b\x24"
        "\x5b\x31\x7e\x6e\x3b\x2a\x78\x3b\x22\x28\x22\x2c\x28\x22\x3b\x22\x2f"
        "\x78\x29\x2c\x22\x29\x22\x5d\x3b\x22\x20\x22\x2f\x78\x5d\x0a\x24\x5b"
        "\x6d\x3c\x23\x78\x3a\x28\x28\x22\x22\x3b\x22\x2c\x22\x29\x28\x31\x7e"
        "\x6e\x29\x29\x2c\x78\x3b\x28\x28\x6d\x2d\x32\x29\x23\x78\x29\x2c\x22"
        "\x2e\x2e\x22\x3b\x78\x5d\x7d\x0a\x74\x3a\x40\x79\x3b\x6b\x3a\x60\x6b"
        "\x78\x79\x20\x78\x0a\x24\x5b\x60\x54\x7e\x74\x3b\x22\x2b\x22\x2c\x64"
        "\x5b\x2b\x79\x3b\x6b\x5d\x3b\x60\x44\x7e\x74\x3b\x64\x5b\x79\x3b\x6b"
        "\x5d\x3b\x30\x7e\x23\x79\x3b\x28\x60\x43\x60\x49\x60\x53\x60\x46\x60"
        "\x5a\x60\x4c\x21\x28\x22\x5c\x22\x5c\x22\x22\x3b\x22\x21\x30\x22\x3b"
        "\x22\x30\x23\x60\x22\x3b\x22\x30\x23\x30\x2e\x22\x3b\x22\x30\x40\x30"
        "\x61\x22\x3b\x22\x28\x29\x22\x29\x29\x74\x3b\x79\x7e\x2a\x79\x3b\x61"
        "\x20\x79\x3b\x76\x5b\x79\x3b\x6b\x3b\x78\x5d\x5d\x7d\x0a\x60\x6b\x3a"
        "\x60\x6b\x78\x79\x20\x31\x30\x30\x30\x30\x30\x30\x0a\x60\x72\x66\x3a"
        "\x20\x7b\x2e\x35\x2b\x28\x78\x3f\x30\x29\x25\x34\x32\x39\x34\x39\x36"
        "\x37\x32\x39\x35\x2e\x7d\x0a\x60\x72\x66\x31\x3a\x7b\x2e\x35\x2b\x28"
        "\x31\x2e\x2b\x78\x3f\x30\x29\x25\x34\x32\x39\x34\x39\x36\x37\x32\x39"
        "\x35\x2e\x7d\x20\x20\x20\x20\x20\x20\x20\x20\x0a\x60\x72\x7a\x3a\x20"
        "\x7b\x28\x25\x2d\x32\x2a\x6c\x6f\x67\x20\x60\x72\x66\x31\x20\x78\x29"
        "\x40\x33\x36\x30\x2e\x2a\x60\x72\x66\x20\x78\x7d\x0a",
        1628);
    F_ = malloc(280 * sizeof(void*));
    F_[0] = (void*)nul;
    F_[1] = (void*)Idy;
    F_[2] = (void*)Flp;
    F_[3] = (void*)Neg;
    F_[4] = (void*)Fst;
    F_[5] = (void*)Sqr;
    F_[6] = (void*)Wer;
    F_[7] = (void*)Rev;
    F_[8] = (void*)Asc;
    F_[9] = (void*)Dsc;
    F_[10] = (void*)Grp;
    F_[11] = (void*)Not;
    F_[12] = (void*)Til;
    F_[13] = (void*)Enl;
    F_[14] = (void*)Srt;
    F_[15] = (void*)Cnt;
    F_[16] = (void*)Flr;
    F_[17] = (void*)Str;
    F_[18] = (void*)Unq;
    F_[19] = (void*)Typ;
    F_[20] = (void*)Val;
    F_[21] = (void*)ech;
    F_[22] = (void*)nyi;
    F_[23] = (void*)rdc;
    F_[24] = (void*)nyi;
    F_[25] = (void*)scn;
    F_[26] = (void*)nyi;
    F_[27] = (void*)lst;
    F_[28] = (void*)Kst;
    F_[29] = (void*)Out;
    F_[30] = (void*)nyi;
    F_[31] = (void*)nyi;
    F_[32] = (void*)Abs;
    F_[33] = (void*)Img;
    F_[34] = (void*)Cnj;
    F_[35] = (void*)Ang;
    F_[36] = (void*)nyi;
    F_[37] = (void*)nyi;
    F_[38] = (void*)nyi;
    F_[39] = (void*)Tok;
    F_[40] = (void*)Fwh;
    F_[41] = (void*)Las;
    F_[42] = (void*)Exp;
    F_[43] = (void*)Log;
    F_[44] = (void*)Sin;
    F_[45] = (void*)Cos;
    F_[46] = (void*)Prs;
    F_[64] = (void*)Asn;
    F_[65] = (void*)Dex;
    F_[66] = (void*)Add;
    F_[67] = (void*)Sub;
    F_[68] = (void*)Mul;
    F_[69] = (void*)Div;
    F_[70] = (void*)Min;
    F_[71] = (void*)Max;
    F_[72] = (void*)Les;
    F_[73] = (void*)Mor;
    F_[74] = (void*)Eql;
    F_[75] = (void*)Mtc;
    F_[76] = (void*)Key;
    F_[77] = (void*)Cat;
    F_[78] = (void*)Cut;
    F_[79] = (void*)Tak;
    F_[80] = (void*)Drp;
    F_[81] = (void*)Cst;
    F_[82] = (void*)Fnd;
    F_[83] = (void*)Atx;
    F_[84] = (void*)Cal;
    F_[85] = (void*)Ech;
    F_[86] = (void*)nyi;
    F_[87] = (void*)Rdc;
    F_[88] = (void*)nyi;
    F_[89] = (void*)Scn;
    F_[90] = (void*)nyi;
    F_[91] = (void*)com;
    F_[92] = (void*)prj;
    F_[93] = (void*)Otu;
    F_[94] = (void*)In;
    F_[95] = (void*)Find;
    F_[96] = (void*)Hyp;
    F_[97] = (void*)Cpx;
    F_[98] = (void*)fdl;
    F_[99] = (void*)Rot;
    F_[100] = (void*)Enc;
    F_[101] = (void*)Dec;
    F_[102] = (void*)nyi;
    F_[103] = (void*)nyi;
    F_[104] = (void*)Bin;
    F_[105] = (void*)Mod;
    F_[106] = (void*)Pow;
    F_[107] = (void*)Lgn;
    F_[193] = (void*)tchr;
    F_[194] = (void*)tnms;
    F_[195] = (void*)tvrb;
    F_[196] = (void*)tpct;
    F_[197] = (void*)tvar;
    F_[198] = (void*)tsym;
    F_[199] = (void*)pop;
    F_[211] = (void*)Amd;
    F_[212] = (void*)Dmd;
    F_[220] = (void*)negi;
    F_[221] = (void*)negf;
    F_[222] = (void*)negz;
    F_[223] = (void*)absi;
    F_[224] = (void*)absf;
    F_[225] = (void*)nyi;
    F_[226] = (void*)addi;
    F_[227] = (void*)addf;
    F_[228] = (void*)addz;
    F_[229] = (void*)mini;
    F_[230] = (void*)minf;
    F_[231] = (void*)minz;
    F_[232] = (void*)muli;
    F_[233] = (void*)mulf;
    F_[234] = (void*)mulz;
    F_[235] = (void*)maxi;
    F_[236] = (void*)maxf;
    F_[237] = (void*)maxz;
    F_[238] = (void*)subi;
    F_[239] = (void*)subf;
    F_[240] = (void*)subz;
    F_[241] = (void*)divi;
    F_[242] = (void*)divf;
    F_[243] = (void*)divz;
    F_[244] = (void*)modi;
    F_[245] = (void*)sqrf;
    F_[246] = (void*)nyi;
    F_[247] = (void*)cmi;
    F_[248] = (void*)cmi;
    F_[249] = (void*)cmi;
    F_[250] = (void*)cmF;
    F_[251] = (void*)cmZ;
    F_[252] = (void*)cmC;
    F_[253] = (void*)cmI;
    F_[254] = (void*)cmI;
    F_[255] = (void*)cmF;
    F_[256] = (void*)cmZ;
    F_[257] = (void*)cmL;
    F_[258] = (void*)sum;
    F_[259] = (void*)rd0;
    F_[260] = (void*)prd;
    F_[261] = (void*)rd0;
    F_[262] = (void*)min;
    F_[263] = (void*)max;
    F_[264] = (void*)mtC;
    F_[265] = (void*)mtC;
    F_[266] = (void*)mtC;
    F_[267] = (void*)mtF;
    F_[268] = (void*)mtF;
    F_[269] = (void*)mtL;
    F_[270] = (void*)inC;
    F_[271] = (void*)inI;
    F_[272] = (void*)inI;
    F_[273] = (void*)inF;
    F_[274] = (void*)inZ;
    F_[275] = (void*)exp1;
    F_[276] = (void*)log1;
    F_[277] = (void*)sin1;
    F_[278] = (void*)cos1;
    F_[279] = (void*)pow2;
}

int main(int args, char** argv)
{
    args_ = (int32_t)args;
    argv_ = argv;
    init();
    main_();
    return 0;
}
