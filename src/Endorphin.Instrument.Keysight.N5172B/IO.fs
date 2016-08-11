// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core
open System.Text

/// Common functions to set/query values of a Keysight instrument.
/// Includes functions to access values such as numbers, frequencies etc.,
/// which are common to different subsystems.
module IO =
    /// Check the model number of the instrument matches one that this software is known to work with.
    let private checkModel instrument = async {
        // not checked - that's not our job in the initialisation bit
        let! identity = SCPI.Query.identity instrument
        match identity.Model with
        | "N1572B" -> ()
        | model    ->
            sprintf "Unexpected RF source model number: %s." model
            |> UnexpectedReplyException
            |> raise }

    /// Post a key to the instrument, then check the error queue afterwards.
    let post key instrument = SCPI.Checked.Set.key key instrument

    /// Set a key to a value, then check the error queue after.
    let set<'In> key (value : 'In) instrument = SCPI.Checked.Set.value key value instrument
    /// Write a sequency of values
    let setSeq<'In> key (values : seq<'In>) instrument =
        SCPI.Checked.Set.value key (String.csvSeqString SCPI.format values) instrument

    /// Query a key for a value, then check the error queue after.
    let query (parser : string -> 'Out) key instrument =
        SCPI.Checked.Query.Key.parsed parser key instrument
    /// Query a key and value for a value, then check the error queue.
    let queryValue<'In, 'Out> (parser : string -> 'Out) key (value : 'In) instrument =
        SCPI.Checked.Query.Value.parsed parser key value instrument
    /// Query a key for a CSV sequence of values, each of which is interpreted by the parser command.
    let querySeq (parser : string -> 'Out) key instrument =
        SCPI.Checked.Query.Key.parsed (String.parseCsvSeq parser) key instrument

    /// The key for setting the units of power.
    let [<Literal>] private powerUnitKey = ":UNIT:POW"
    /// The base for querying an amplitude from the machine.
    let private amplitudeBase parser key instrument = async {
        // get the currently set power unit
        let! unit = query string powerUnitKey instrument
        do! set powerUnitKey "DBM" instrument
        let! result = query parser key instrument
        do! set powerUnitKey unit instrument }

    /// Safely query an amplitude from the machine, setting the power units to be
    /// in the correct format before, and returning them to their previous settings
    /// afterwards.
    let queryAmplitude key instrument = amplitudeBase Parse.amplitude key instrument
    /// Safely query an amplitude sequence from the machine, setting the power units to be
    /// in the correct format before, and returning them to their previous settings
    /// afterwards.
    let queryAmplitudeSeq key instrument = amplitudeBase (String.parseCsvSeq Parse.amplitude) key instrument

    /// Perform initialisation checks on the machine to ensure the driver software will work with it.
    /// Throws SCPI.InvalidResponseException if the machine responds with an invalid string over SCPI,
    /// UnexpectedReplyException if the model does not match, or InstrumentErrorException if there are
    /// errors in the queue.
    let initialise instrument = async {
        let checks = [ checkModel ; SCPI.Checked.Query.errors ]
        for check in checks do
            do! check instrument }

    /// Return control back to the front panel of the instrument.
    let localControl instrument = post ":SYSTEM:COMM:GTLocal" instrument

    /// Connect to an Agilent E8257D over TCPIP Visa, and perform the initialisation checks.
    let connect visaTcpipAddress timeout = async {
        let instrument = Visa.openTcpipInstrument visaTcpipAddress timeout None
        do! initialise instrument
        return instrument }

    /// Disconnect from an Agilent E8257D, returning control back to the front panel.
    let disconnect instrument = async {
        do! localControl instrument
        do! Visa.closeInstrument instrument }
