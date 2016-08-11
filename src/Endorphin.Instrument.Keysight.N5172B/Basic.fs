// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core

[<AutoOpen>]
module Basic =
    /// Key for the carrier wave freqeuncy of the device.
    /// Command reference p.44.
    let private carrierFrequencyKey = ":FREQUENCY"
    /// Set the carrier wave frequency of the device.
    let setCarrierFrequency = fun x -> IO.set<Frequency> carrierFrequencyKey x
    /// Query the carrier wave frequency of the device.
    let queryCarrierFrequency = fun x -> IO.query Parse.frequency carrierFrequencyKey x

    /// Key for the RF amplitude of the machine.
    /// Command reference p.83.
    let private carrierAmplitudeKey = ":POWER"
    /// Set the RF amplitude of the carrier wave.
    let setCarrierAmplitude = fun x -> IO.set<Amplitude> carrierAmplitudeKey x
    /// Query the RF amplitude of the carrier wave.
    let queryCarrierAmplitude = fun x -> IO.queryAmplitude carrierAmplitudeKey x

    /// Key for the phase of the modulating signal.
    /// Command reference p.49.
    let private phaseKey = ":PHASE"
    /// Set the phase of the modulation signal on the machine.
    let setPhase = fun x -> IO.set<Phase> phaseKey x
    /// Query the phase of the modulation signal of the machine.
    let queryPhase = fun x -> IO.query Parse.phase phaseKey x

    /// Key to send a trigger on the bus.
    /// Command reference p.124.
    let private triggerKey = "*TRG"
    /// Send a trigger on the bus.
    let trigger = fun x -> IO.post triggerKey x

    /// Key for the overall RF output state. Must be On if anything is to play
    /// Command reference p.157.
    let private outputStateKey = ":OUTP:STAT"
    /// Sets the RF output on or off.
    let setOutput = fun x -> IO.set<OnOffState> outputStateKey x
    /// Queries whether the RF output is on or off.
    let queryOutput = fun x -> IO.query Parse.onOffState outputStateKey x