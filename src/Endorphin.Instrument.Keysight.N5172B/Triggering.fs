// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core

module Triggering =
    /// Get the key prefix needed for different types of trigger.
    let triggerTypePrefix = function
        | StepTrigger -> ""
        | ListTrigger -> ":LIST"

    module Control =
        /// Key for the type of the trigger source.
        /// Command reference p.60 for list triggers.
        let private sourceTypeKey trigger = sprintf "%s:TRIGGER:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of trigger source, given a trigger type.
        let setSourceType trigger = IO.set<TriggerSourceType> (sourceTypeKey trigger)
        /// Query the type of trigger source, given a trigger type.
        let querySourceType trigger = IO.query Parse.triggerSourceType (sourceTypeKey trigger)

        /// Key for the type of external trigger source.
        /// Command reference p.58.
        let private externalSourceKey trigger = sprintf "%s:TRIGGER:EXTERNAL:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of external trigger source, given a trigger type.
        let setExternalSource trigger = IO.set<ExternalTriggerSource> (externalSourceKey trigger)
        /// Query the value of the external trigger source, given a trigger type.
        let queryExternalSource trigger = IO.query Parse.externalTriggerSource (externalSourceKey trigger)

        /// Key for the external slope parity.
        /// Command reference p.59.
        let private externalSlopePolarityKey trigger = sprintf "%s:TRIGGER:SLOPE" (triggerTypePrefix trigger)
        /// Set the external slope polarity of the given trigger type to the given value.
        let setExternalSlopePolarity trigger = IO.set<Polarity> (externalSlopePolarityKey trigger)
        /// Query the external slope polarity of the given trigger type.
        let queryExternalSlopePolarity trigger = IO.query Parse.polarity (externalSlopePolarityKey trigger)

        /// Key for the internal trigger source.
        /// Command reference p.59.
        let private internalSourceKey trigger = sprintf "%s:TRIGGER:INTERNAL:SOURCE" (triggerTypePrefix trigger)
        /// Set the type of internal trigger source, given a trigger type.
        let setInternalSource trigger = IO.set<InternalTriggerSource> (internalSourceKey trigger)
        /// Query the value of the internal trigger source, given a trigger type.
        let queryInternalSource trigger = IO.query Parse.internalTriggerSource (internalSourceKey trigger)

        /// Key for the period of the timer trigger.
        /// Command reference p.215.
        let private timerPeriodKey trigger = sprintf "%s:TRIGGER:TIMER" (triggerTypePrefix trigger)
        /// Set the period of the timer trigger for the given trigger.
        let setTimerPeriod trigger = IO.set<Duration> (timerPeriodKey trigger)
        /// Query the period of the timer trigger for the given trigger.
        let queryTimerPeriod trigger = IO.query Parse.duration (timerPeriodKey trigger)

        /// Set the trigger source of the machine, given a type of trigger and a value to set
        /// the source to.
        let setTriggerSource trigger triggerSource instrument = async {
            match triggerSource with
            | Immediate  -> do! setSourceType trigger ImmediateType instrument
            | TriggerKey -> do! setSourceType trigger TriggerKeyType instrument
            | Bus        -> do! setSourceType trigger BusType instrument
            | TriggerSource.External (source, polarity) ->
                do! setSourceType trigger ExternalType instrument
                do! setExternalSource trigger source instrument
                do! setExternalSlopePolarity trigger polarity instrument
            | Internal source ->
                do! setSourceType trigger InternalType instrument
                do! setInternalSource trigger source instrument
            | Timer period ->
                do! setSourceType trigger TimerType instrument
                do! setTimerPeriod trigger period instrument }
