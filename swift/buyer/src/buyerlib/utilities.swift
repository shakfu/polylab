import Foundation

public func today() -> String {
    // Date() get's today's date and time already
    let date = Date()
    let formatter = DateFormatter()
    formatter.dateFormat = "yyyy-MM-dd"
    let result = formatter.string(from: date)
    return result
}
