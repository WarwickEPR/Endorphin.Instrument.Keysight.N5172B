// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#I "../../packages"

#r "Endorphin.Core/lib/net452/Endorphin.Core.dll"
#r "Endorphin.Core.NationalInstruments/lib/net452/Endorphin.Core.NationalInstruments.dll"
#r "bin/Debug/Endorphin.Instrument.Keysight.N5172B.dll"


// I don't open any modules throughout to demonstrate exactly where each function comes from.
// Most (if not all) modules may be opened to simplify this, but in general I suggest you don't
// do that due to naming clashes.

// It's ok to open the "Control" module, though - due to the nature of the module compared to
// other modules, it's unlikely to have any naming collisions.

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight.N5172B
open Endorphin.Core

// log4net.Config.BasicConfigurator.Configure ()

// a phase for use with RF pulses - this sets I and Q to have the same
// relative power
let equalIQ = Phase.single (Phase_rad 0.0<rad>)

// Define the pulse sequence.  We can use up to three marker channels (even though there are four), because
// Endorphin needs one internally for the RF blanking pulse.

// At the default (currently unchangeable in experiment mode) ARB clock rate, one sample is equal to
// 1/(150Mhz), or 6.67ns.
let pulses halfpi tau0 deltatau = seq {
    yield Pulse.rf equalIQ         halfpi        // pi/2 rf pulse
    yield Pulse.delayWithIncrement tau0 deltatau // tau delay
    yield Pulse.rf equalIQ         (halfpi * 2u) // pi rf pulse
    yield Pulse.delayWithIncrement tau0 deltatau // tau delay
    yield Pulse.trigger Markers.marker1 }        // trigger acquisition via marker 1

// define parameters to do with the experiment
let experiment =
    pulses 60u 6000u 100u
    |> Experiment.create
    |> Experiment.withRepetitions 128
    |> Experiment.withShotRepetitionTime 10e-6<s>

// define the routing of the marker channels
let routing =
    Routing.empty
    |> Routing.withBasebandTrigger1 RouteMarker1
    |> Routing.withBasebandTrigger2 RouteMarker2
    |> Routing.withEvent1           RouteMarker3
    // markers default to positive polarity, but make this explicit
    |> Routing.withMarker1Polarity  Positive
    |> Routing.withMarker2Polarity  Positive
    |> Routing.withMarker3Polarity  Positive

try
    async {
        // open the box keysight - set the VISA access string you need here and timeout
        let! keysight = IO.connect "USB0::0x0957::0x1F01::MY53051252::INSTR" 100000<ms>

        do! Routing.set routing keysight
        do! ARB.Trigger.set (ARB.Trigger.continuous FreeRun) keysight

        // set up rf carrier
        do! setCarrierFrequency (Frequency_Hz 150e6<Hz>) keysight
        do! setCarrierAmplitude (Power_dBm 4.0<dBm>) keysight

        // store the experiment on the machine and play
        let! storedExperiment = Control.Experiment.store experiment keysight
        do! Control.Experiment.playStored storedExperiment keysight

        // tidy up and close
        do! IO.disconnect keysight }
    |> Async.RunSynchronously
with
    | :? SCPI.InstrumentErrorException as exn -> printfn "Failed with instrument errors: %A" exn.Data0