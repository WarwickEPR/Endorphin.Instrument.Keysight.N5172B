// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

/// Functions to translate between internal and machine representations.
module internal Parse =
    /// Convert a string containing a number into a power in dBm.
    let amplitude (str : string) = Power_dBm (float str * 1.0<dBm>)

    /// Convert a string containing a number into a frequence in Hz.
    let frequency (str : string) = Frequency_Hz (float str * 1.0<Hz>)

    /// Convert a string containing a number into a phase in radians.
    let phase (str : string) = Phase_rad (float str * 1.0<rad>)

    /// Convert a string containing a number into a duration in seconds.
    let duration (str : string) = Duration_sec (float str * 1.0<s>)

    /// Convert a string containing a number into a percentage.
    let percentage (str : string) = Percentage (float str * 1.0<pct>)

    /// Convert a string containing a number into a decibel ratio in dB.
    let decibelRatio (str : string) = DecibelRatio (float str * 1.0<dB>)

    /// Convert a machine representation of an impedance into an internal representation.
    let impedance = function
        | "50"      -> Impedance_50Ohm
        | "600"     -> Impedance_600Ohm
        | "1000000" -> Impedance_1MOhm
        | str       -> raise << UnexpectedReplyException <| sprintf "Unexpected impedance string: %s." str

    /// Convert a machine representation of a direction into an internal representation.
    let direction str =
        match String.toUpper str with
        | "UP"   -> Up
        | "DOWN" -> Down
        | _      -> raise << UnexpectedReplyException <| sprintf "Unexpected direction string: %s." str

    /// Convert a machine representation of a coupling into an internal representation.
    let coupling str =
        match String.toUpper str with
        | "AC" -> AC
        | "DC" -> DC
        | _    -> raise << UnexpectedReplyException <| sprintf "Unexpected coupling string: %s." str

    /// Convert a machine representation of an on/off state into an internal representation.
    let onOffState str =
        match String.toUpper str with
        | "0" | "OFF" -> Off
        | "1" | "ON"  -> On
        | str         -> raise << UnexpectedReplyException <| sprintf "Unexpected on-off string: %s." str

    /// Convert a machine representation of an automatic/manual state into an
    /// internal representation.
    let autoManualState str =
        match String.toUpper str with
        | "AUTO"           -> Auto
        | "MAN" | "MANUAL" -> Manual
        | _                -> raise << UnexpectedReplyException <| sprintf "Unexpected auto-manual string: %s." str

    /// Convert a machine representation of a polarity into an internal representation.
    let polarity str =
        match String.toUpper str with
        | "POS" | "POSITIVE" -> Positive
        | "NEG" | "NEGATIVE" -> Negative
        | _                  -> raise << UnexpectedReplyException <| sprintf "Unexpected trigger polarity string: %s." str

    /// Convert a machine representation of a low/high state into an internal
    /// representation.
    let lowHighState str =
        match String.toUpper str with
        | "LOW" -> Low
        | "HIGH" -> High
        | _ -> raise << UnexpectedReplyException <| sprintf "Unexpected low/high state string: %s" str

    /// Convert the machine representation of a sweep mode into the internal representation.
    let sweepMode str =
        match String.toUpper str with
        | "CW"
        | "FIX"
        | "FIXED" -> Sweep.SweepMode.Fixed
        | "LIST"  -> Sweep.SweepMode.Swept
        | str     -> raise << UnexpectedReplyException <| sprintf "Unexpected sweep mode string: %s." str

    /// Convert the machine representation of the sweep type to the internal representation.
    let sweepType str =
        match String.toUpper str with
        | "LIST" -> Sweep.SweepType.List
        | "STEP" -> Sweep.SweepType.Step
        | _      -> raise << UnexpectedReplyException <| sprintf "Unexpected sweep type string: %s." str

    /// Convert the machine representation of the step spacing into the internal representation.
    let stepSpacing str =
        match String.toUpper str with
        | "LIN" | "LINEAR"      -> LinearStepSpacing
        | "LOG" | "LOGARITHMIC" -> LogarithmicStepSpacing
        | _                     -> raise << UnexpectedReplyException <| sprintf "Unexpected step spacing string: %s." str

    /// Convert a machine representation of an external trigger source to an internal
    /// representation.
    let externalTriggerSource str =
        match String.toUpper str with
        | "TRIG1" | "TRIGGER1" -> Trigger1
        | "TRIG2" | "TRIGGER2" -> Trigger2
        | "PULS" | "PULSE"     -> Pulse
        | _                    -> raise << UnexpectedReplyException
                                  <| sprintf "Unexpected external trigger source string: %s" str

    /// Convert a machine representation of an internal trigger source to an internal
    /// representation.
    let internalTriggerSource str =
        match String.toUpper str with
        | "PVID" | "PVIDEO" -> PulseVideo
        | "PSYN" | "PSYNC"  -> PulseSync
        | _                 -> raise << UnexpectedReplyException
                               <| sprintf "Unexpected trigger source string: %s" str

    /// Convert a machine representation of a trigger source into an internal representation.
    let triggerSourceType str =
        match String.toUpper str with
        | "IMM" | "IMMEDIATE" -> ImmediateType
        | "KEY"               -> TriggerKeyType
        | "BUS"               -> BusType
        | "EXT" | "EXTERNAL"  -> ExternalType
        | "INT" | "INTERNAL"  -> InternalType
        | "TIM" | "TIMER"     -> TimerType
        | _                   -> raise << UnexpectedReplyException
                                 <| sprintf "Unexpected trigger source type string: %s." str
    /// Convert the machine representation of a trigger source into an internal representation.
    let sourceProvider str =
        match String.toUpper str with
        | "EXT1" -> ExternalPort EXT1
        | "EXT2" -> ExternalPort EXT2
        | "FUNCTION1" -> InternalGenerator Function1
        | str -> raise << UnexpectedReplyException <| sprintf "Unexpected source: %s" str

    /// Convert the machine representation of the depth type into the internal
    /// representation.
    let depthType str =
        match String.toUpper str with
        | "LIN" | "LINEAR" -> LinearType
        | "EXP" | "EXPONENTIAL" -> ExponentialType
        | str -> raise << UnexpectedReplyException <| sprintf "Unexpected depth type: %s" str

    /// If a type claiming to provide an interface we're using is passed, but we don't know about
    /// it, we have to fail execution.
    let internal failIncorrectType signal =
        sprintf "Unexpected output signal in interface %A, %A" (signal.GetType()) signal
        |> UnexpectedReplyException
        |> raise

    /// Convert a machine representation of a marker signal into an internal representation.
    let markerSignal str =
        match String.toUpper str with
        | "M1"   -> RouteMarker1 :> IMarkerSignal
        | "M2"   -> RouteMarker2 :> IMarkerSignal
        | "M3"   -> RouteMarker3 :> IMarkerSignal
        | "M4"   -> RouteMarker4 :> IMarkerSignal
        | "NONE" -> NoSignal     :> IMarkerSignal
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpted marker signal string: %s" str

    /// Convert a machine representation of a user output route into an internal representation.
    let userSignal str =
        match String.toUpper str with
        | "M1"    -> RouteMarker1 :> IUserSignal
        | "M2"    -> RouteMarker2 :> IUserSignal
        | "M3"    -> RouteMarker3 :> IUserSignal
        | "M4"    -> RouteMarker4 :> IUserSignal
        | "AUX29" -> RouteAux29   :> IUserSignal
        | "NONE"  -> NoSignal     :> IUserSignal
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected user output signal string: %s" str

    /// Convert a machine representation of a sweep out routing into an internal representation.
    let sweepOutSignal str =
        match String.toUpper str with
        | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ISweepOutSignal
        | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ISweepOutSignal
        | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ISweepOutSignal
        | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ISweepOutSignal
        | "SWE" | "SWEEP"     -> RouteSweepOut          :> ISweepOutSignal
        | "SRUN"              -> RouteSweepRun          :> ISweepOutSignal
        | "NONE"              -> NoSignal               :> ISweepOutSignal
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected sweep out signal string: %s" str

    /// Convert a machine representation of a trigger routing into an internal representation.
    let triggerSignal str =
        match String.toUpper str with
        | "SETT" | "SETTLED"  -> RouteSourceSettled     :> ITriggerSignal
        | "PVID" | "PVIDEO"   -> RoutePulseVideo        :> ITriggerSignal
        | "PSYN" | "PSYNC"    -> RoutePulseSync         :> ITriggerSignal
        | "SFD" | "SFDONE"    -> RouteSweptFunctionDone :> ITriggerSignal
        | "SWE" | "SWEEP"     -> RouteSweepTriggerOut   :> ITriggerSignal
        | "LXI"               -> RouteLxi               :> ITriggerSignal
        | "PULS" | "PULSE"    -> RoutePulseBnc          :> ITriggerSignal
        | "TRIG" | "TRIGGER1" | "TRIGGER2"
                              -> RouteOtherTrigger      :> ITriggerSignal
        | "NONE"              -> NoSignal               :> ITriggerSignal
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected trigger signal string: %s" str

    /// Convert a machine representation of a user BNC routing into an internal representation.
    let userBncSignal str =
        match String.toUpper str with
        | "BBTR" | "BBTR1" | "BBTRIGGER" | "BBTRIGGER1"
                                 -> RouteBasebandTrigger1 :> IUserBncSignal
        | "BBTR2" | "BBTRIGGER2" -> RouteBasebandTrigger2 :> IUserBncSignal
        | "EVEN" | "EVEN1" | "EVENT" | "EVENT1"
                                 -> RouteEvent1  :> IUserBncSignal
        | "PTR" | "PTRIGGER"     -> RoutePatternTrigger :> IUserBncSignal
        | "NONE"                 -> NoSignal     :> IUserBncSignal
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected user BNC signal string: %s" str

    /// Convert a machine representation of the continuous type mode of the dual ARB triggering
    /// system into an internal representation.
    let arbContinuousMode str =
        match String.toUpper str with
        | "FREE"             -> FreeRun
        | "TRIG" | "TRIGGER" -> TriggerThenRun
        | "RES"  | "RESET"   -> ResetOnTrigger
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected ARB continuous mode trigger type string: %s" str

    /// Covnert a machine representaiton of the single trigger retrigger mode of the dual ARB
    /// trigger system into an internal representation.
    let arbRetriggerMode str =
        match String.toUpper str with
        | "ON"  | "1" -> BufferedRetrigger
        | "OFF" | "0" -> NoRetrigger
        | "IMM" | "IMMEDIATE" -> RestartRetrigger
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected dual ARB retrigger string: %s" str

    /// Convert a machine representation of the segment advance type mode of the dual ARB
    /// triggering system into a machine representation.
    let arbSegmentAdvanceMode str =
        match String.toUpper str with
        | "SING" | "SINGLE" -> ArbSegmentAdvanceMode.SinglePlay
        | "CONT" | "CONTINUOUS" -> ContinuousPlay
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected ARB segment advance mode trigger type string: %s" str

    /// Convert a machine representation of the physical location of an external dual ARB
    /// trigger into an internal representation.
    let arbExternalConnector str =
        match String.toUpper str with
        | "EPT1" | "EPTRIGGER1" -> ArbBnc
        | "EPT2" | "EPTRIGGER2" -> ArbAux
        | _ -> raise << UnexpectedReplyException
               <| sprintf "Unexpected ARB external trigger source location string: %s" str
