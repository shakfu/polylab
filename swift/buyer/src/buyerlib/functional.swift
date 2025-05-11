func pipe<T>(arg: T, fs: [(T) -> T]) -> T {
    var x: T = arg
    for f in fs {
        x = f(x)
    }
    return x
}

// Copyright (c) 2014 Rob Rix. All rights reserved.
// see: https://github.com/robrix/Prelude/blob/master/Prelude/Compose.swift

precedencegroup Composition {
    /// Function composition is associative, but since we want to chain compositions, we pick right-associativity primarily for consistency with Haskell.
    associativity: right

    // This is a higher precedence than the exponentiative operators `<<` and `>>`.
    higherThan: BitwiseShiftPrecedence
}

infix operator >>>: Composition

// MARK: - Left-to-right composition

/// Returns the left-to-right composition of unary `g` on unary `f`.
///
/// This is the function such that `(f >>> g)(x)` = `g(f(x))`.
public func >>> <T, U, V>(f: @escaping (T) -> U, g: @escaping (U) -> V) -> (T) -> V {
    return { g(f($0)) }
}

/// Returns the left-to-right composition of unary `g` on binary `f`.
///
/// This is the function such that `(f >>> g)(x, y)` = `g(f(x, y))`.
public func >>> <T, U, V, W>(f: @escaping (T, U) -> V, g: @escaping (V) -> W) -> (T, U) -> W {
    return { g(f($0, $1)) }
}

/// Returns the left-to-right composition of binary `g` on unary `f`.
///
/// This is the function such that `(f >>> g)(x, y)` = `g(f(x), y)`.
public func >>> <T, U, V, W>(f: @escaping (T) -> U, g: @escaping (U, V) -> W) -> (T, V) -> W {
    return { g(f($0), $1) }
}

// ---------------------------------------------------------------------------
// my fork of above to resemble the R '%>%' semantics: works great!!!

infix operator %>%: Composition

/// the simple case of appication x %>% f = f(x)
public func %>% <T, U>(arg: T, f: (T) -> U) -> U {
    return f(arg)
}

/// This is the function such that `(f %>% g)(x)` = `g(f(x))`.
public func %>% <T, U, V>(f: @escaping (T) -> U, g: @escaping (U) -> V) -> (T) -> V {
    return { g(f($0)) }
}
