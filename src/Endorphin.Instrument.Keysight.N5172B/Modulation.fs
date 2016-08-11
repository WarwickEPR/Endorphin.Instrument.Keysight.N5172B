// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core
open Source

module Modulation =
    /// Get the extra infix part of the key needed for the modulation source.
    let modulationSourceInfix = function
        | ExternalSource _ -> ""
        | InternalSource _ -> ":INTERNAL"

    module Control =
        /// Key needed to set the state of the modulation.
        let private stateKey = ":STATE"
        /// Key needed to set the source of the modulation.
        let private sourceKey = ":SOURCE"

        module Amplitude =
            /// Generate the prefix needed for a certain key based on the subsystem path.
            let private prefix (path : AmPath) key = sprintf ":%s%s" (SCPI.format path) key

            /// Set the state of the amplitude modulation of the given path.
            let setState path = IO.set<OnOffState> (prefix path stateKey)
            /// Query the state of the amplitude modulation of the given path.
            let queryState path = IO.query Parse.onOffState (prefix path stateKey)

            /// Set the source of the amplitude modulation of the given path.
            let setSource path = IO.set<SourceProvider> (prefix path sourceKey)
            /// Query the source of the amplitude modulation of the given path.
            let querySource path = IO.query Parse.sourceProvider (prefix path sourceKey)

            /// Key needed to set the type of the amplitude modulation.
            let private typeKey path = prefix path ":TYPE"
            /// Set the type of the ampltiude modulation of the given path.
            let internal setType path = IO.set<DepthType> (typeKey path)
            /// Query the type of the amplitude modulation of the given path.
            let internal queryType path = IO.query Parse.depthType (typeKey path)

            /// Key needed for operations on linear depth.
            let private depthLinearKey path = prefix path ":DEPTH"
            /// Set the depth of the given path to be a percentage.
            let private setDepthLinear path = IO.set<Percentage> (depthLinearKey path)
            /// Query the depth of the given path, and parse the result as a percentage.
            let private queryDepthLinear path = IO.query Parse.percentage (depthLinearKey path)

            /// Key needed for operations on exponential depth.
            let private depthExponentialKey path = prefix path ":DEPTH:EXPONENTIAL"
            /// Set the depth of the given path to as a decibel ratio.
            let private setDepthExponential path = IO.set<DecibelRatio> (depthExponentialKey path)
            /// Query the depth of the given path, and parse the result as a decibel ratio.
            let private queryDepthExponential path = IO.query Parse.decibelRatio (depthExponentialKey path)

            /// Set the depth of the given path to the given value.
            let setDepth path depth instrument =
                match depth with
                | Linear v -> setDepthLinear path v instrument
                | Exponential v -> setDepthExponential path v instrument

        module Frequency =
            /// Create the prefix necessary for to apply the key to the correct path.
            let private prefix (path : FmPath) key = sprintf ":%s%s" (SCPI.format path) key

            /// Set the state of the frequency modulation of the given path.
            let setState path = IO.set<OnOffState> (prefix path stateKey)
            /// Query the state of the frequency modulation of the given path.
            let queryState path = IO.query Parse.onOffState (prefix path stateKey)

            /// Set the source of the frequency modulation of the given path.
            let setSource path = IO.set<SourceProvider> (prefix path sourceKey)
            /// Query the source of the frequency modulation of the given path.
            let querySource path = IO.query Parse.sourceProvider (prefix path sourceKey)

            /// Key needed for operations on frequency deviation.
            let private deviationKey path = prefix path ":DEVIATION"
            /// Set the deviation of the frequency modulation of the given path.
            let setDeviation path = IO.set<Frequency> (deviationKey path)
            /// Query the deviation of the frequency modulation of the given path.
            let queryDeviation path = IO.query Parse.frequency (deviationKey path)

    module Runtime =
        /// Key needed to set the output modulation state of the machine.
        let private modulationStateKey = ":OUTPUT:MODULATION"
        /// Set the output modulation state of the machine.
        let setModulationState = fun x -> IO.set<OnOffState> modulationStateKey x
        /// Query the output modulation state of the machine.
        let queryModulationState = fun x -> IO.query Parse.onOffState modulationStateKey x

    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        /// A set of default function settings, which can be changed through the constructor
        /// functions.
        let defaultFunctionSettings = {
            Shape = Sine
            Frequency = Frequency_Hz 1.0e3<Hz>
            PhaseOffset = Phase_rad 0.0<rad> }
        /// Change the shape of the given settings to match the new value.
        let withShape shape (settings : FunctionSettings) = { settings with Shape = shape }
        /// Change the frequency (in Hz) of the given settings to match the new value.
        let withFrequencyInHz frequency (settings : FunctionSettings) =
            { settings with Frequency = Frequency_Hz frequency }
        /// Change the phase offset (in radians) of the given settings to match the
        /// new value.
        let withPhaseOffsetInRad phase (settings : FunctionSettings) =
           { settings with PhaseOffset = Phase_rad phase }
        /// Create a new sine-shaped function generator source with the given frequency in Hz.
        let internalSineSourceInHz frequency =
            InternalSource (Function1, defaultFunctionSettings |> withFrequencyInHz frequency)

        /// Create a new function generator source with the given frequence (in Hz), shape, and
        /// phase (in radians).
        let internalGeneralSourceInHz frequency shape phase =
            let settings = defaultFunctionSettings
                           |> withShape shape
                           |> withFrequencyInHz frequency
                           |> withPhaseOffsetInRad phase
            InternalSource (Function1, settings)

    module Apply =
        open Control
        open Endorphin.Core

        /// Verify that the given modulation settings are valid, producing a choice failure if
        /// they are not.
        let private verifyModulationSettings settings = choice {
            let duplicateChannels = settings |> List.map modulationChannel |> List.duplicates
            let duplicateSources  = settings |> List.map modulationSource |> List.duplicates
            // TODO: when PM is added, check that PM and FM paths are exclusive
            if not duplicateChannels.IsEmpty then
                do! Choice.fail << System.ArgumentException
                                << sprintf "Repeated modulation channels: %s"
                                << List.prettyPrint
                                << List.map SCPI.format
                                <| duplicateChannels
            if not duplicateSources.IsEmpty then
                do! Choice.fail << System.ArgumentException
                                << sprintf "Modulation sources used more than once: %s"
                                << List.prettyPrint
                                << List.map (sourceProvider >> SCPI.format)
                                <| duplicateSources
            return! Choice.succeed () }

        /// Apply a given modulation to the machine.
        let private applyModulation modulation instrument = async {
            match modulation with
            | AmplitudeModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| SCPI.format path
                let sourcePrefix = prefix + modulationSourceInfix source
                do! Amplitude.setDepth path settings.Depth instrument
                do! Source.Apply.setup sourcePrefix source instrument
                do! Amplitude.setSource path (sourceProvider source) instrument
                do! Amplitude.setState path On instrument
            | FrequencyModulation (path,settings,source) ->
                let prefix = sprintf ":%s" <| SCPI.format path
                let sourcePrefix = prefix + modulationSourceInfix source
                do! Frequency.setDeviation path settings.Deviation instrument
                do! Source.Apply.setup sourcePrefix source instrument
                do! Frequency.setSource path (sourceProvider source) instrument
                do! Frequency.setState path On instrument}

        /// Apply a list of modulation settings to the machine in order, after first
        /// verifying them.
        let modulationSettings settings instrument = async {
            Choice.bindOrRaise <| verifyModulationSettings settings
            for modulation in settings do
                do! applyModulation modulation instrument }
