// import AudioKit
// import Foundation

// public func demo_audiokit() {
//     do {
//         let engine = AudioEngine()
//         let osc = Oscillator()
//         let env = AmplitudeEnvelope(osc)
//         let delay = Delay(env)
//         env.attackDuration = 0.01
//         env.decayDuration = 0.2
//         env.sustainLevel = 0.2
//         env.releaseDuration = 0.2
//         delay.time = 0.3  // seconds
//         delay.feedback = 0.8  // Normalized Value 0 - 1
//         delay.dryWetMix = 0.3  // Normalized Value 0 - 1
//         engine.output = delay
//         try engine.start()
//         osc.start()
//         osc.frequency = AUValue.random(in: 220...880)
//         env.start()
//         delay.start()
//         // usleep(1000000) //will sleep for 1 second
//         sleep(2)
//         osc.stop()
//         env.stop()
//         delay.stop()
//         engine.stop()
//     } catch let error {
//         print("audikit demo failed: \(error)")
//     }
// }

// public func demo_audiokit1() {
//     do {
//         let engine = AudioEngine()
//         let osc = Oscillator()
//         engine.output = osc
//         try engine.start()
//         osc.start()
//         osc.frequency = AUValue.random(in: 220...880)
//         // usleep(1000000) //will sleep for 1 second
//         sleep(2)
//         osc.stop()
//         engine.stop()
//     } catch let error {
//         print("audikit demo failed: \(error)")
//     }
// }
