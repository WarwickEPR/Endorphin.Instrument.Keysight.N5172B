// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core

module Sweep =
    /// Extract the string identifier from a stored sweep type.
    let storedSweepString (StoredSweep str) = str
    /// Convert an identifier into a StoredString identifier.
    let toStoredSweep str = StoredSweep str

    module Control =
        [<AutoOpen>]
        module Frequency =
            /// Key to set the frequency sweep mode.
            /// Command reference p.45.
            let private frequencySweepModeKey = ":FREQUENCY:MODE"
            /// Set the frequency sweep mode to the given type.
            let setFrequencySweepMode = fun x -> IO.set<SweepMode> frequencySweepModeKey x
            /// Query the frequency sweep mode the machine is currently in.
            let queryFrequencySweepMode = fun x -> IO.query Parse.sweepMode frequencySweepModeKey x

            /// Key needed to set the first value in a frequency sweep.
            /// Command reference p.48.
            let private startFrequencyKey = ":FREQUENCY:START"
            /// Set the first value in a frequency sweep.
            let setStartFrequency = fun x -> IO.set<Frequency> startFrequencyKey x
            /// Query the currently set first value of the frequency sweep.
            let queryStartFrequncy = fun x -> IO.query Parse.frequency startFrequencyKey x

            /// Key needed to set the end value of the frequency sweep.
            /// Command reference p.48.
            let private stopFrequencyKey = ":FREQUENCY:STOP"
            /// Set the value of the end frequency in a frequency sweep.
            let setStopFrequency = fun x -> IO.set<Frequency> stopFrequencyKey x
            /// Query the currently set last value of the frequency sweep.
            let queryStopFrequency = fun x -> IO.query Parse.frequency stopFrequencyKey x

            /// Key needed to set the length of the frequency range for a step sweep.
            /// Command reference p.48.
            let private frequencySpanKey = ":FREQUENCY:SPAN"
            /// Set the range of a frequency step sweep.
            let setFrequencySpan = fun x -> IO.set<Frequency> frequencySpanKey x
            /// Query the currently set range of a frequency step sweep.
            let queryFrequencySpan = fun x -> IO.query Parse.frequency frequencySpanKey x

        [<AutoOpen>]
        module Amplitude =
            /// Key needed for the amplitude sweep type.
            /// Command reference p.84.
            let private amplitudeSweepModeKey = ":POWER:MODE"
            /// Set the type of the amplitude sweep.
            let setAmplitudeSweepMode = fun x -> IO.set<SweepMode> amplitudeSweepModeKey x
            /// Query the currently set type of the amplitude sweep.
            let queryAmplitudeSweepMode = fun x -> IO.query Parse.sweepMode amplitudeSweepModeKey x

            /// Key needed for setting the first value in an amplitude sweep,
            /// Command reference p.86.
            let private startAmplitudeKey = ":POWER:START"
            /// Set the first value of an amplitude sweep.
            let setStartAmplitude = fun x -> IO.set<Amplitude> startAmplitudeKey x
            /// Query the currently set first value in an amplitude sweep.
            let queryStartAmplitude = fun x -> IO.queryAmplitude startAmplitudeKey x

            /// Key needed to set the end amplitude in an amplitude sweep.
            /// Command reference p.86.
            let private stopAmplitudeKey = ":POWER:STOP"
            /// Set the final value of an amplitude sweep.
            let setStopAmplitude = fun x -> IO.set<Amplitude> stopAmplitudeKey x
            /// Query the currently set value of the amplitude sweep.
            let queryStopAmplitude = fun x -> IO.queryAmplitude stopAmplitudeKey x

        /// Key needed to set the type of the sweep.
        let private typeKey = ":LIST:TYPE"
        /// Set the sweep type to either List or Step.
        let setSweepType = fun x -> IO.set<SweepType> typeKey x
        /// Query the current sweep type of the machine.
        let querySweepType = fun x -> IO.query Parse.sweepType typeKey x

        /// Key needed to set the direction of the sweep.
        /// Command reference p.54.
        let private directionKey = ":LIST:DIRECTION"
        /// Set direction of sweep from start -> stop for Up and stop -> start for Down.
        let setDirection = fun x -> IO.set<Direction> directionKey x
        /// Query the currently set direction of the sweep.
        let queryDirection = fun x -> IO.query Parse.direction directionKey x

        /// Key needed to set whether sweeps will repeat continuously or simply stop.
        /// Command reference p.211.
        let private continuousModeKey = ":INITIATE:CONTINUOUS"
        /// Set whether sweeps should immediately repeat, or wait for some trigger to begin
        /// replaying.
        let setContinousMode = fun x -> IO.set<OnOffState> continuousModeKey x
        /// Query whterh sweeps are set to immediately repeat, or if they need some trigger
        /// to begin replaying.
        let queryContinuousMode = fun x -> IO.query Parse.onOffState continuousModeKey x

        /// Key needed to set the sweep mode to either automatic (sweep), or manual (select a
        /// single point from a sweep.
        /// Command reference p.56.
        let private modeKey = ":LIST:MODE"
        /// Sets manual or automatic mode for progressing through points of a sweep.
        let setMode = fun x -> IO.set<AutoManualState> modeKey x
        /// Query whether the current sweep mode is in automatic or manual selection.
        let queryMode = fun x -> IO.query Parse.autoManualState modeKey x

        /// Key needed to set the dwell time of each sweep step.
        /// Command reference p.63.
        let private dwellTimeKey = ":SWEEP:DWELL"
        /// Set the time to stay on each point of a sweep for.
        let setDwellTime = fun x -> IO.set<Duration> dwellTimeKey x
        /// Query the current time taken for each sweep point.
        let queryDwellTime = fun x -> IO.query Parse.duration dwellTimeKey x

        /// Key needed to turn attentuation protection on and off.
        let private attenuationProtectionKey = ":SWEEP:ATTEN:PROTECTION"
        /// Set the state of the attentuation protection for frequency and power step sweeps.
        /// Disabling allows the sweep to more optimally set the attenuation leveling control
        /// and the output attentuation at each sweep point, but sets the dwell time to a minimum
        /// of 50ms for safety.
        let setAttenuationProtection = fun x -> IO.set<OnOffState> attenuationProtectionKey x
        /// Query the current state of the attentuation protection.
        let queryAttenuationProtection = fun x -> IO.query Parse.onOffState attenuationProtectionKey x

        /// Key needed to set whether to remain on the last point of a sweep, or return to the
        /// first point.
        let private retraceKey = ":LIST:RETRACE"
        /// Set whether to retrace back to the first point of a sweep (On), or remain at the end
        /// point of the sweep (Off).
        let setRetrace = fun x -> IO.set<OnOffState> retraceKey x
        /// Query whether the machine will retrace back to the first point in a sweep (On), or if
        /// it will remain at the end (Off).
        let queryRetrace = fun x -> IO.query Parse.onOffState retraceKey x

        /// Apply the given sweep options to the machine.
        let internal setSweepOptions options instrument = async {
            do! setDirection options.Direction instrument
            match options.StepTrigger with
            | Some trig -> do! Triggering.Control.setTriggerSource StepTrigger trig instrument
            | None -> ()
            match options.ListTrigger with
            | Some trig -> do! Triggering.Control.setTriggerSource ListTrigger trig instrument
            | None -> ()
            match options.DwellTime with
                | Some t -> do! setDwellTime t instrument
                | None   -> if options.ListTrigger = Some Immediate
                            then return raise <| UnexpectedReplyException "Dwell time required for free-running, immediate trigger sweep through a list of points"
            do! setRetrace options.Retrace instrument
            do! setAttenuationProtection options.AttentuationProtection instrument
            do! setMode options.Mode instrument }

        module Step =
            /// Key needed to set the step spacing of sweeps.
            /// Command reference p.65.
            let private stepSpacingKey = ":SWEEP:SPACING"
            /// Set the step spacing of sweeps to be either logarithmic or linear.
            let setStepSpacing = fun x -> IO.set<StepSpacing> stepSpacingKey x
            /// Query whether sweeps are set to use logarithmic or linear spacing.
            let queryStepSpacing = fun x -> IO.query Parse.stepSpacing stepSpacingKey x

            /// Key needed to set the number of points in a sweep.
            /// Command reference p.65.
            let private pointsCountKey = ":SWEEP:POINTS"
            /// Set the number of points in a sweep to an integer between 2 and 65535.
            let setPointsCount = fun x -> IO.set<int> pointsCountKey x
            /// Query how many points the sweep is set up to use.
            let queryPointsCount = fun x -> IO.query int pointsCountKey x

            /// Set the frequency sweep to use the given frequency sweep settings.
            let setFrequencySweep frequencySweep instrument = async {
                match frequencySweep with
                | FixedFrequency f ->
                    do! setCarrierFrequency f instrument
                    do! setFrequencySweepMode Fixed instrument
                | FrequencySweep sweep ->
                    do! Frequency.setStartFrequency sweep.Start instrument
                    do! Frequency.setStopFrequency sweep.Stop instrument
                    do! setFrequencySweepMode Swept instrument }

            /// Set the amplitude sweep to use the given amplitude sweep settings.
            let setAmplitudeSweep amplitudeSweep instrument = async {
                match amplitudeSweep with
                | FixedAmplitude a ->
                    do! setCarrierAmplitude a instrument
                    do! setAmplitudeSweepMode Fixed instrument
                | AmplitudeSweep sweep ->
                    do! Amplitude.setStartAmplitude sweep.Start instrument
                    do! Amplitude.setStopAmplitude sweep.Stop instrument
                    do! setAmplitudeSweepMode Swept instrument }

        module List =
            /// Key needed to set the time to dwell on each point in a list sweep.
            /// Command reference p.54.
            let private dwellTimesKey = ":LIST:DWELL"
            /// Set a sequence of times to dwell on each point in a list sweep.
            let setDwellTimes key = fun x -> IO.setSeq<Duration> dwellTimesKey key x
            /// Query the currently set sequence of times to dwell on each point in a list sweep.
            let queryDwellTimes = fun x -> IO.querySeq Parse.duration dwellTimesKey x

            /// Key needed to find out how many different dwell times are set.
            /// Command reference p.54.
            let private dwellTimesCountKey = ":LIST:DWELL:POINTS"
            /// Query how many dwell times are currently set for the sweep.
            let queryDwellTimesCount = fun x -> IO.query int dwellTimesCountKey x

            /// Key needed to set the dwell type from list or step sweeps.
            /// Command reference p.55.
            let private dwellTypeKey = ":LIST:DWELL:TYPE"
            /// Set the dwell type to either list or step sweeps.
            let setDwellType = fun x -> IO.set<SweepMode> dwellTypeKey x
            /// Query the currently set dwell type (either list or step).
            let queryDwellType = fun x -> IO.query Parse.sweepMode dwellTypeKey x

            /// Key needed to set a list of amplitudes for the current sweep points.
            /// Command reference p.57.
            let private powersKey = ":LIST:POWER"
            /// Set the amplitudes of the current list sweep points.
            let setPowers key = fun x -> IO.setSeq<Amplitude> powersKey key x
            /// Query the amplitudes of the current list sweep points.
            let queryPowers = fun x -> IO.queryAmplitudeSeq powersKey x

            /// Key needed to find how many amplitudes are in the current list sweep file.
            /// Command reference p.58.
            let private powersCountKey = ":LIST:POWER:POINTS"
            /// Query how many amplitudes are in the current list sweep file.
            let queryPowersCount = fun x -> IO.query int powersCountKey x

            /// Key needed to set a list of frequencies for the current sweep points.
            /// Command reference p.55.
            let private frequenciesKey = ":LIST:FREQUENCY"
            /// Set the frequencies of the current list sweep points.
            let setFrequencies key = fun x -> IO.setSeq<Frequency> frequenciesKey key x
            /// Query the frequencies of the current list sweep points.
            let queryFrequencies = fun x -> IO.querySeq Parse.frequency frequenciesKey x

            /// Key needed to find how many frequencies are in the current list sweep file.
            /// Command reference p.55.
            let private frequenciesCountKey = ":LIST:FREQUENCY:POINTS"
            /// Query how many frequencies are in the current list sweep file.
            let queryFrequenciesCount = fun x -> IO.query int frequenciesCountKey x

            /// Key needed to add waveform entries to the current list sweep.
            let private listWaveformKey = ":LIST:WAVEFORM"
            /// Set a single waveform as an element of a list sweep.
            let setListWaveform = fun x -> IO.set<StoredWaveform> listWaveformKey x

            /// Set a sequence of waveforms as elements of a list sweep.
            let setListWaveformSequence = fun x -> IO.setSeq<StoredWaveform> listWaveformKey x

            /// Key needed to load a list file from memory.
            /// Command reference p.149, p.154.
            let private loadListKey = ":MMEM:LOAD:LIST"
            /// Load a list file into the current list sweep settings.
            let loadListFile = fun x -> IO.set<StoredSweep> loadListKey x

            /// Key needed to store a list file into memory.
            /// Command reference p.150, p.155.
            let private storeListKey = ":MMEM:STORE:LIST"
            /// Store the currently loaded list sweep into a file in the non-volatile memory of the
            /// machine, so it can be reloaded later.
            let storeListFileById id instrument = async {
                do! IO.set<string> storeListKey (Filename.string Filename.listFolder id) instrument
                return toStoredSweep id }

    /// Commands used to control running sweeps.
    module Runtime =
        /// Starts an armed sweep waiting on Bus triggering.
        let busTrigger = fun x -> IO.post "*TRG" x

        /// Key needed to begin sweeping immediately.
        /// Command reference p.215.
        let private immediateKey = ":TRIGGER"
        /// Starts an armed sweep immediately without waiting for selected trigger event.
        let immediate = fun x -> IO.post immediateKey x

        /// Key needed to get the current sweep point.
        /// Command reference p.63.
        let private currentPointKey = ":SWEEP:CPOINT"
        /// Reports current point in List/Step sequence.
        let queryCurrentPoint = fun x -> IO.query int currentPointKey x

        /// Key needed to immediately abort a sweep.
        /// Command reference p.211.
        let private abortKey = ":ABORT"
        /// Abort current sweep. In continous mode, arms a new sweep.
        let abort = fun x -> IO.post abortKey x

        /// Aborts current sweep and rearms. With immediate triggering starts a new sweep.
        let abortAndRearmSingle = fun x -> IO.post ":TSWEEP" x

        /// Key needed to rearm a single sweep.
        /// Command reference p.211.
        let private rearmSingleKey = ":INITIATE"
        /// Arms a single sweep. In immediate mode, starts immediately.
        let rearmSingle = fun x -> IO.post rearmSingleKey x

    /// Build a configuration using the data model.
    module Configure =
        open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

        /// Create an absolute value of fixed amplitude.
        let fixedPowerInDbm power = FixedAmplitude <| Power_dBm power
        /// Create an amplitude sweep between two values, both in absolute dBm.
        let powerSweepInDbm a b = AmplitudeSweep <| range (Power_dBm a) (Power_dBm b)

        /// Create a fixed frequency value in Hz.
        let fixedFrequencyInHz frequency = FixedFrequency <| Frequency_Hz frequency
        /// Create a frequency sweep between two values of frequency, both in Hz.
        let frequencySweepInHz a b = FrequencySweep <| range (Frequency_Hz a) (Frequency_Hz b)

        /// A default set of options for a sweep.  These are the values the machine uses after a
        /// (*RST) command.
        let internal defaultSweepOptions = {
            Direction = Up
            StepTrigger = Some Immediate
            ListTrigger = Some Immediate
            DwellTime = Some ( Duration_sec 2e-3<s> )
            Retrace = On
            AttentuationProtection = On
            Mode = Auto }

        /// A default step sweep, using the values the machine would default to if issued the reset
        /// (*RST) command.
        let private defaultStepSweep = {
            Frequency = fixedFrequencyInHz 1e9<Hz>
            Amplitude = fixedPowerInDbm -110.0<dBm>
            Points = 101
            Spacing = LinearStepSpacing
            Options = defaultSweepOptions }

        /// Create a step sweep with the given points.
        let withPoints points (config : StepSweep) = { config with Points = points }

        /// Create a step sweep with the given spacing.
        let withSpacing spacing config = { config with Spacing = spacing }

        /// Create a step sweep with the given sweep direction.
        let withDirection direction config =
            { config with Options = { config.Options with Direction = direction } }

        /// Create a step sweep with the given sweep dwell time.
        let withDwellTime time config =
            { config with Options = { config.Options with DwellTime = time } }

        /// Create a step sweep with a given step trigger source.
        let withStepTrigger trigger config =
            { config with Options = { config.Options with StepTrigger = trigger } }

        /// Create a step sweep with a given list trigger source.
        let withListTrigger trigger config =
            { config with Options = { config.Options with ListTrigger = trigger } }

        /// Create a step sweep with retracing set to the specified state.
        let withRetrace state config =
            { config with Options = { config.Options with Retrace = state } }

        /// Create a step sweep with attenuation protection set to the specified state.
        let withAttenuationProtection state config =
            { config with Options = { config.Options with AttentuationProtection = state } }

        /// Create a step sweep with a fixed absolute power in dBm.
        let withFixedPowerInDbm power config =
            { config with StepSweep.Amplitude = fixedPowerInDbm power }
        /// Create a step sweep with a fixed frequency in Hz.
        let withFixedFrequencyInHz frequency config =
            { config with StepSweep.Frequency = fixedFrequencyInHz frequency }
        /// Create a step sweep with a frequency sweep between the specified start and end frequencies,
        /// measured in Hz.
        let frequencyStepSweepInHz start finish =
            { defaultStepSweep with Frequency = frequencySweepInHz start finish }
        /// Create a step sweep with an amplitude sweep between the specified start and end
        /// absolute amplitudes, measured in dBm.
        let powerStepSweepInDbm start finish =
            { defaultStepSweep with Amplitude = powerSweepInDbm start finish }

        /// Set the StepTrigger in a sweep options record.
        let internal optionsWithStepTrigger value options =
            { options with StepTrigger = value }

        /// Set the ListTrigger in a sweep options record.
        let internal optionsWithListTrigger value options =
            { options with ListTrigger = value }

        /// Set the dwell time in a sweep options record.
        let internal optionsWithDwellTime value options =
            { options with DwellTime = value }

        /// Set the state of the retrace in a step sweep options.
        let internal optionsWithRetrace value options =
            { options with Retrace = value }

    // Apply a configuration.
    module Apply =
        open Control
        /// Set up an RF step sweep from a model.
        let stepSweep (stepSweep : StepSweep) instrument = async {
            do! Step.setFrequencySweep stepSweep.Frequency instrument
            do! Step.setAmplitudeSweep stepSweep.Amplitude instrument
            do! Step.setPointsCount stepSweep.Points instrument
            do! Step.setStepSpacing stepSweep.Spacing instrument
            do! setSweepOptions stepSweep.Options instrument
            do! setSweepType Step instrument }