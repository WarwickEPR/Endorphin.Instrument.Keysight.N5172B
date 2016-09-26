// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

#I "../../packages"

#r "Endorphin.Core/lib/net452/Endorphin.Core.dll"
#r "Endorphin.Core.NationalInstruments/lib/net452/Endorphin.Core.NationalInstruments.dll"
#r "bin/Debug/Endorphin.Instrument.Keysight.N5172B.dll"

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Endorphin.Instrument.Keysight.N5172B
open Endorphin.Core

open Sweep.Configure
open Modulation.Configure

let ext1 = ExternalSource (EXT1,{ Coupling = AC; Impedance = Impedance_50Ohm })

let pi = System.Math.PI
let fun2 = internalGeneralSourceInHz 1.0e5<Hz> (Ramp Positive) (pi*0.5<rad>)
let fun3 = internalGeneralSourceInHz 1.0e5<Hz> (Ramp Positive) (degreesToRadians 90.0<deg>)

let functionSource frequency = InternalSource ( Function1,
                                                { Shape = Sine
                                                  Frequency = frequency
                                                  PhaseOffset = Phase_rad 0.0<rad> } )

let test = { Depth = depthInPercentage (Percentage 50.0<pct>) }

let am = AmplitudeModulation (AM1, { Depth = depthInPercentage (Percentage 50.0<pct>) }, ext1)
let fm = FrequencyModulation (FM2, { Deviation = Frequency_Hz 2.0e3<Hz> }, fun2)

let modulationSettings = [ am ; fm ]


let sweepExperiment startFrequency stopFrequency =
    async {
        let! keysight = IO.connect "TCPIP0::192.168.1.2" 3000<ms>
        //let keysight = Dummy.openDumbInstrument
        let! identity = SCPI.Query.identity (IO.scpiInstrument keysight)
        printf "%A" identity

        let sweepSettings =
            frequencyStepSweepInHz startFrequency stopFrequency
            |> withPoints 200
            |> withFixedPowerInDbm -3.0<dBm>
            |> withDwellTime (Some (Duration_sec 1e-2<s>))

        printfn "\nSetting up experiment:\n%A" sweepSettings
//        do! Sweep.Apply.stepSweep keysight sweepSettings

        let keysightRfSettings1 = { Sweep = (StepSweep sweepSettings)
                                    Modulation = modulationSettings }
        let keysightRfSettings2 = { Sweep = NoSweep <| ((Frequency_Hz 1.2e9<Hz>),(Power_dBm 0.1<dBm>))
                                    Modulation = [] }

    //    do! RfSource.setModulationState keysight Off
    //    do! RfSource.Frequency.setCwFrequency keysight (FrequencyInHz 1.0e9<Hz>)

        do! RfSource.applySettings keysightRfSettings1 keysight

        //let! amplitude = queryCwAmplitude keysight
        //printfn "%A" amplitude

        do! IO.disconnect keysight }


let out = sweepExperiment 1.0e9<Hz> 2.0e9<Hz>
           |> Async.RunSynchronously
