//
//  ContentView.swift
//  swuidemo
//
//  Created by sa on 2/28/21.
//

import SwiftUI

func myprint() {
    print("HELLO DOLLY!")
}

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, World!")
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            Button("Hit Me") {
                myprint()
            }
        }
    }
}


//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView()
//    }
//}
