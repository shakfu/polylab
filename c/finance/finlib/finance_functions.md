# Finance functions

Formulas implemented in `finlib.c`. Argument names match `finlib.h`.

## Conventions

- `r` is the interest rate per period, as a fraction: 5% is 0.05.

- `n` is the number of periods. `r` and `n` must use the same period. For monthly payments at 6% a year over 30 years, use `r = 0.005` and `n = 360`.

- Suffixes:

  - no suffix: payments fall at the end of each period (ordinary annuity).

  - `b`: payments fall at the beginning of each period (annuity due).

  - `l`: a single lump sum, not a series of payments.

- All amounts are positive. There is no Excel-style sign convention for money in versus money out.

- Several formulas divide by `r`. At `r = 0` the library returns the limit:

  - `fv`, `fvb`, `pv`, `pvb`: `payment * n`

  - `pmt`, `pmtb`: `(loan - balloon) / n`

  - `nper`, `nperb`: `loan / payment`

- Inputs with no solution return NaN or inf.

## Choosing a function

| Question | Function |
|-|-|
| What will regular savings grow to? | `fv`, `fvb` |
| What will a single deposit grow to? | `fvl` |
| What is a stream of future payments worth today? | `pv`, `pvb` |
| What is one future amount worth today? | `pvl` |
| What is an uneven series of cash flows worth today? | `npv`, `npvb` |
| What is the payment on a loan? | `pmt`, `pmtb` |
| How long until a loan is repaid? | `nper`, `nperb` |
| How long until a deposit reaches a target? | `nperl` |
| What growth rate turns one amount into another? | `ratel` |
| How much does an asset depreciate each period? | `sln`, `syd`, `ddb` |

Use the `b` variant when each payment is made at the start of its period. Rent, leases and insurance premiums are usually paid in advance. Loan repayments, bond coupons and salaries are usually paid in arrears.

## Future value

### fv(rate, n, payment)

$$\mathrm{fv} = \text{payment} \cdot \frac{(1 + r)^n - 1}{r}$$

Value after `n` periods of a fixed payment made at the end of each period. Each payment earns interest from the period after it is made.

Use it to project a savings plan or a pension contribution schedule.

Example: 200 saved at the end of each month for 10 years at 0.5% a month. `fv(0.005, 120, 200)` = 32,775.87.

### fvb(rate, n, payment)

$$\mathrm{fvb} = \text{payment} \cdot \frac{\left((1 + r)^n - 1\right)(1 + r)}{r}$$

As `fv`, with each payment made at the start of its period. Every payment earns one more period of interest, so the result is `fv * (1 + r)`.

Use it when contributions go in at the start of the period, such as a deposit made on payday.

Example: the same plan with deposits at the start of each month. `fvb(0.005, 120, 200)` = 32,939.75.

### fvl(rate, n, principal)

$$\mathrm{fvl} = \text{principal} \cdot (1 + r)^n$$

Value after `n` periods of a single amount invested now, with interest compounded each period.

Use it for a fixed-term deposit, or to project any one-off investment.

Example: 10,000 invested for 10 years at 6%. `fvl(0.06, 10, 10000)` = 17,908.48.

## Present value

### pv(rate, n, payment)

$$\mathrm{pv} = \text{payment} \cdot \frac{1 - (1 + r)^{-n}}{r}$$

Value today of `n` fixed payments received at the end of each period.

Use it to price an annuity, compare a pension against a lump sum, or value the coupon stream of a bond.

Example: 1,000 a year for 20 years, discounted at 4%. `pv(0.04, 20, 1000)` = 13,590.33.

### pvb(rate, n, payment)

$$\mathrm{pvb} = \text{payment} \cdot \frac{\left(1 - (1 + r)^{-n}\right)(1 + r)}{r}$$

As `pv`, with each payment received at the start of its period. The first payment is not discounted, so the result is `pv * (1 + r)`.

Use it to value a lease or rent paid in advance.

Example: the same 20 payments, each made at the start of the year. `pvb(0.04, 20, 1000)` = 14,133.94.

### pvl(rate, n, future)

$$\mathrm{pvl} = \text{future} \cdot (1 + r)^{-n}$$

Value today of a single amount received after `n` periods. This is the inverse of `fvl`.

Use it to price a zero-coupon bond, or to find how much to set aside now for a known future cost.

Example: 50,000 needed in 5 years, at 5%. `pvl(0.05, 5, 50000)` = 39,176.31.

## Net present value

Both functions take an array `c` of `len` cash flows, which may differ in size and sign. They differ only in when the first cash flow falls.

### npv(rate, cashflows, len)

$$\mathrm{npv} = \sum_{i=0}^{len-1} \frac{c_i}{(1 + r)^{i+1}}$$

Value today of cash flows at the end of periods 1, 2, ..., `len`. The first cash flow is discounted by one period. Excel's `NPV` uses the same convention.

Use it when every cash flow lies in the future. To value a project, add the initial outlay separately.

Example: 3,000, 4,000 and 5,000 at the end of years 1 to 3, at 8%. `npv(0.08, {3000, 4000, 5000}, 3)` = 10,176.29.

### npvb(rate, cashflows, len)

$$\mathrm{npvb} = \sum_{i=0}^{len-1} \frac{c_i}{(1 + r)^{i}}$$

As `npv`, with the first cash flow at time 0 and left undiscounted.

Use it for investment appraisal, where `c[0]` is the outlay today. For an outlay followed by inflows, a positive result means the project earns more than `r`. `irr` returns the rate at which `npvb` is 0.

Example: pay 10,000 now for the cash flows above. `npvb(0.08, {-10000, 3000, 4000, 5000}, 4)` = 176.29, so the project clears an 8% hurdle rate.

## Loan payments

### pmt(rate, n, loan, balloon)

$$\mathrm{pmt} = \frac{\left(\text{loan} - \text{balloon} \cdot (1 + r)^{-n}\right) r}{1 - (1 + r)^{-n}}$$

Fixed payment at the end of each period that reduces `loan` to `balloon` after `n` periods. Set `balloon = 0` to repay the loan in full.

Use it for mortgage or car loan repayments. Use a nonzero `balloon` for balloon loans and finance deals with a final lump payment.

Examples:
- 300,000 mortgage over 25 years at 0.4% a month. `pmt(0.004, 300, 300000, 0)` = 1,718.99 a month.

- 30,000 car loan over 4 years at 0.5% a month, with 10,000 due at the end. `pmt(0.005, 48, 30000, 10000)` = 519.70 a month.

### pmtb(rate, n, loan, balloon)

$$\mathrm{pmtb} = \frac{\left(\text{loan} - \text{balloon} \cdot (1 + r)^{-n}\right) r}{\left(1 - (1 + r)^{-n}\right)(1 + r)}$$

As `pmt`, with each payment at the start of its period. The result is `pmt / (1 + r)`.

Use it for leases, where the first payment is due at signing.

Example: lease a 20,000 car for 3 years at 0.5% a month, with a residual value of 8,000. `pmtb(0.005, 36, 20000, 8000)` = 403.05 a month.

## Number of periods

### nper(rate, payment, loan)

$$\mathrm{nper} = -\log_{1+r}\left(1 - \frac{\text{loan} \cdot r}{\text{payment}}\right)$$

Number of end-of-period payments needed to repay `loan`. This is `pv` solved for `n`. The result is usually fractional; the last payment is then smaller than the others.

Use it to see how overpaying shortens a loan.

The payment must exceed the interest charged in the first period, `loan * r`. If it equals that interest, the loan is never repaid and the result is inf. If it is smaller, the balance grows and the result is NaN.

Example: repay 20,000 at 500 a month, at 0.5% a month. `nper(0.005, 500, 20000)` = 44.74 months.

### nperb(rate, payment, loan)

$$\mathrm{nperb} = -\log_{1+r}\left(1 - \frac{\text{loan} \cdot r}{\text{payment} \cdot (1 + r)}\right)$$

As `nper`, with each payment at the start of its period. This is `pvb` solved for `n`.

Example: the same loan with payments in advance. `nperb(0.005, 500, 20000)` = 44.49 months.

### nperl(rate, target, principal)

$$\mathrm{nperl} = -\log_{1+r}\left(\frac{\text{principal}}{\text{target}}\right)$$

Number of periods for `principal` to grow to `target` at rate `r`. This is `fvl` solved for `n`.

Use it to find how long an investment takes to reach a goal, such as doubling.

Example: double 10,000 at 7% a year. `nperl(0.07, 20000, 10000)` = 10.24 years. The rule of 72 estimates 72 / 7 = 10.3.

## Rate

### ratel(n, target, principal)

$$\mathrm{ratel} = \left(\frac{\text{target}}{\text{principal}}\right)^{1/n} - 1$$

Rate per period at which `principal` grows to `target` in `n` periods. This is `fvl` solved for `r`, also called the compound annual growth rate (CAGR) when periods are years.

Use it to compare investments held for different lengths of time, or to state growth in revenue or prices as a yearly rate.

Example: 10,000 grew to 20,000 in 10 years. `ratel(10, 20000, 10000)` = 0.0718, or 7.18% a year.

## Depreciation

Depreciation spreads the cost of an asset, less its salvage value, over its useful life. The three methods differ in how much they charge in each period. `sln` and `syd` always charge `cost - salvage` in total. `ddb` does only if book value reaches salvage.

Example asset: cost 10,000, salvage 1,000, life 5 years.

| Year | `sln` | `syd` | `ddb` |
|-|-|-|-|
| 1 | 1,800 | 3,000 | 4,000 |
| 2 | 1,800 | 2,400 | 2,400 |
| 3 | 1,800 | 1,800 | 1,440 |
| 4 | 1,800 | 1,200 | 864 |
| 5 | 1,800 | 600 | 296 |
| Total | 9,000 | 9,000 | 9,000 |

### sln(cost, salvage, life)

$$\mathrm{sln} = \frac{\text{cost} - \text{salvage}}{\text{life}}$$

Straight-line depreciation: the same charge in every period.

Use it when an asset is used evenly over its life, such as a building or office furniture. It is the simplest method.

### syd(cost, salvage, life, period)

$$\mathrm{syd} = \frac{(\text{cost} - \text{salvage})(\text{life} - \text{period} + 1)}{\text{life}(\text{life} + 1)/2}$$

Sum-of-years'-digits depreciation for one period. The charges fall by the same amount each period. The denominator is the sum `1 + 2 + ... + life`. Returns 0 for a period outside `[1, life]`.

Use it for assets that lose more value when new, such as vehicles, where straight line would overstate book value in the early years.

### ddb(cost, salvage, life, period)

$$d_k = \min\left(\frac{2 B_{k-1}}{\text{life}},\ B_{k-1} - \text{salvage}\right), \qquad B_0 = \text{cost}, \quad B_k = B_{k-1} - d_k$$

Double-declining-balance depreciation for one period. Each period charges `2 / life` of the current book value `B`. The charge never takes book value below salvage. Returns 0 for a period outside `[1, life]`.

Use it for assets that lose value fastest, such as computers. It charges the most in the first period of the three methods. US tax depreciation (MACRS) uses the same 200% declining-balance rate for many asset classes, switching to straight line when that gives a larger charge ([IRS Publication 946](https://www.irs.gov/publications/p946)).

The charge falls by a fixed fraction each period, so book value may never reach a low salvage value. With salvage 0, the example asset still has a book value of 777.60 after 5 years. `ddb` does not switch to straight line to close that gap.
