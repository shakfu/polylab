import Foundation

func fullstop(str: String) -> String {
    let res = str + "."
    return res
}

func upper(str: String) -> String {
    let res = str.uppercased()
    return res
}

func replace(str: String) -> String {
    str.replacingOccurrences(of: "world", with: "sunshine")
}

func process(f: (Int, Int) -> Int) -> Int {
    let res = f(10, 2)
    print(res)
    return res
}

let doubler = { (x: Int) -> Int in
    2 * x
}

let result = process(f: { $0 * $1 * 2 })

public func demo_feature() {
    let f1 = replace >>> upper >>> fullstop
    print(f1("hello world"))

    let s = pipe(arg: "the world is warm", fs: [replace, upper, fullstop])
    print(s)

    func g(x: Int) -> Int { x + 2 }
    func p(x: Any) { print(x) }
    func f(x: Int, y: Int) -> (Int) -> Int { { $0 + x + y } }

    // can also be multi-line like r but swift-format makes it one line
    10 %>% { $0 * 100 } %>% g %>% f(x: 10, y: 20) %>% p
}
