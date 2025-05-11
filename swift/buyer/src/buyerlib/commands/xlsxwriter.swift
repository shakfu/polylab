import Foundation
import cxlsxwriter

public func demo_cxlsxwriter() {
    print("demo here")

    var expenses = [(item: String, cost: Float)]()

    expenses.append(("milk", 3.02))
    expenses.append(("eggs", 2.45))
    expenses.append(("juice", 4.99))
    expenses.append(("cereal", 8.89))

    print("the expenses are:", expenses)

    /* Create a workbook and add a worksheet. */
    let workbook: UnsafeMutablePointer<lxw_workbook> = workbook_new("demo.xlsx")

    let worksheet: UnsafeMutablePointer<lxw_worksheet> = workbook_add_worksheet(workbook, nil)

    // defer {
    //     workbook_close(workbook)
    // }

    var row: UInt32 = 0

    worksheet_write_number(worksheet, 0, 0, 5.5, nil)

    // Iterate over the data and write it out element by element.
    for (index, obj) in expenses.enumerated() {
        row = UInt32(index)
        worksheet_write_string(worksheet, row, 0, obj.item, nil)
        worksheet_write_number(worksheet, row, 1, Double(obj.cost), nil)
    }

    // Write a total using a formula.
    let totals_row = UInt32(expenses.count)
    worksheet_write_string(worksheet, totals_row, 0, "Total", nil)
    worksheet_write_formula(worksheet, totals_row, 1, "=SUM(B1:B4)", nil)

    // /* Save the workbook and free any allocated memory. */
    // // return workbook_close(workbook)
    workbook_close(workbook)
}
