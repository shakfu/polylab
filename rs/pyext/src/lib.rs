
use pyo3::prelude::*;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn sum_as_string(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}


/// Adds two integers
#[pyfunction]
fn add(a: i32, b: i32) -> PyResult<i32> {
    Ok(a+b)
}

/// A Python module implemented in Rust.
#[pymodule]
fn pyext(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(sum_as_string, m)?)?;
    m.add_function(wrap_pyfunction!(add, m)?)?;
    Ok(())
}