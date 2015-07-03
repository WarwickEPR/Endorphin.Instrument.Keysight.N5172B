﻿#r @"..\packages\log4net.2.0.3\lib\net40-full\log4net.dll"
#r @"..\packages\ExtCore.0.8.45\lib\net45\ExtCore.dll"
#r @"..\Endorphin.Core\bin\Debug\Endorphin.Core.dll"
#r "NationalInstruments.Common.dll"
#r "NationalInstruments.VisaNS.dll"
#r @"bin\Debug\Endorphin.Instrument.KeysightRfSource.dll"

open Endorphin.Instrument.Keysight
open log4net.Config
open ExtCore.Control
open System

open IQData.Control

BasicConfigurator.Configure()

let printResult =
    function
    | Success ()    -> printfn "Successfully did things."
    | Failure error -> printfn "Bad things happened: %s" error

let amplitude = 32000.0
let numSamples = 600
let fractionalSin frac = Math.Sin (2.0 * Math.PI * frac)
let fractionalCos frac = Math.Cos (2.0 * Math.PI * frac)

let testWaveform = { Name = "test"
                     Data = seq {for i in 1 .. numSamples -> { Sample.I = int16 (amplitude * fractionalSin ((double (i-1))/(double numSamples)))
                                                               Sample.Q = int16 (amplitude * fractionalCos ((double (i-1))/(double numSamples)))
                                                               Sample.Marker1 = Convert.ToBoolean((i-1)%2)
                                                               Sample.Marker2 = Convert.ToBoolean(i%2)
                                                               Sample.Marker3 = Convert.ToBoolean((i-1)%3) 
                                                               Sample.Marker4 = Convert.ToBoolean(0) } } }

let writeTest = asyncChoice {
    let! keysight = RfSource.openInstrument "TCPIP0::192.168.1.2" 3000
    let! identity = RfSource.queryIdentity keysight
    printfn "%A" identity

    do! writeVolatileWaveformFile keysight testWaveform
    do! writeVolatileMarkerFile keysight testWaveform
    do RfSource.closeInstrument |> ignore }

let out = writeTest |> Async.RunSynchronously

printResult out