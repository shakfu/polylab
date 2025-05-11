import XCTest

import buyerlibTests
import buyerTests

var blibtests = [XCTestCaseEntry]()
blibtests += buyerlibTests.allTests()
XCTMain(blibtests)


var btests = [XCTestCaseEntry]()
btests += buyerTests.allTests()
XCTMain(btests)
