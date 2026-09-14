import doctest

import finance
import pytest


def test_doctests():
    assert doctest.testmod(finance).failed == 0


@pytest.mark.parametrize("cashflows, expected", [
    ([-100.0, 60.0, 60.0, 60.0], 0.36309653947517),
    ([-100.0, 10.0, 10.0, 10.0], -0.42441744383163),
    ([0.0, -100.0, 150.0], 0.5),
    ([-1.0] + [1e6] * 200, 1e6),
])
def test_irr_is_npv_root(cashflows, expected):
    rate = finance.irr(cashflows)
    assert rate == pytest.approx(expected)
    assert finance.npv(rate, cashflows) == pytest.approx(0.0, abs=1e-6)


@pytest.mark.parametrize("cashflows, expected", [
    ([-100.0, 100.0], 0.0),
    ([-100.0, 200.0], 1.0),
    ([-100.0, 300.0], 2.0),
    ([-100.0, 50.0], -0.5),
    ([100.0, -200.0], 1.0),
    ([100.0, -50.0], -0.5),
    ([-100.0, 0.0, 400.0], 1.0),
    ([-100.0, 150.0], 0.5),
    ([-100.0, 125.0], 0.25),
    ([-100.0, 75.0], -0.25),
])
def test_irr_exact_roots_are_exact(cashflows, expected):
    assert finance.irr(cashflows) == expected


@pytest.mark.parametrize("cashflows", [
    [], [-100.0], [0.0, 60.0, 60.0], [100.0, 60.0, 60.0], [-100.0, 200.0, -50.0],
])
def test_irr_rejects_undefined_or_multiple(cashflows):
    with pytest.raises(ValueError):
        finance.irr(cashflows)


def test_analysis_marks_undefined_metrics_none():
    r = finance.investment_analysis(0.1, [-100.0, 200.0, -150.0])
    assert r["payback"] == 0.5 and r["irr"] is None and not r["approve"]
    r = finance.investment_analysis(0.1, [-100.0, 60.0, 30.0])
    assert r["payback"] is None and r["irr"] is not None and not r["approve"]


@pytest.mark.parametrize("cashflows, dips", [
    ([-100.0, 150.0, 50.0], False),
    ([-100.0, 100.0, 0.0], False),
    ([-100.0, 60.0, 30.0], False),
    ([-100.0, 200.0, -150.0], True),
    ([-100.0, 100.0, -50.0, 60.0, 10.0], True),
])
def test_analysis_flags_payback_dips(cashflows, dips):
    results = finance.investment_analysis(0.1, cashflows)
    assert results["payback_dips"] is dips
    report = finance.format_report(0.1, cashflows, results)
    assert ("warning: cumulative cashflow later dips" in report) is dips


def test_report_formats_signed_currency():
    cashflows = [-100.0, 99.4]
    results = finance.investment_analysis(0.0, cashflows)
    report = finance.format_report(0.0, cashflows, results)
    assert "NPV: -0.60" in report
    assert "IRR: -0.60%" in report
    assert "==> Do Not Approve investment of 100.00" in report


def test_report_aligns_wide_year_columns():
    cashflows = [-1.0] + [1.0] * 11
    results = finance.investment_analysis(0.05, cashflows)
    report = finance.format_report(0.05, cashflows, results)
    years, amounts = report.splitlines()[1:3]
    assert years.index("11") == amounts.rindex("1.00")


def test_main_prints_report(capsys):
    finance.main(["0.05", "-100", "150", "50"])
    out = capsys.readouterr().out
    assert "Payback: 0.67 years" in out
    assert "==> Approve investment of 100.00" in out


@pytest.mark.parametrize("argv", [
    [], ["0.05"], ["abc", "-100", "60"], ["0.1", "100", "60"], ["-1", "-100", "60"],
])
def test_main_usage_errors_exit_2_without_report(argv, capsys):
    with pytest.raises(SystemExit) as exc:
        finance.main(argv)
    assert exc.value.code == 2
    assert capsys.readouterr().out == ""
