import Foundation
import Stencil

public func demo_stencil() {
    let template = Template(templateString: "Hello {{ name }}")
    do {
        let output: String = try template.render(["name": "socrates"])
        print(output)
    } catch let error {
        print("error: \(error)")
    }

}
