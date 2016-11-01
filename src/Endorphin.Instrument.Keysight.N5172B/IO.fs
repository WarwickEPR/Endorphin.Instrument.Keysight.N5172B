// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core
open System.Text

/// Common functions to set/query values of a Keysight instrument.
/// Includes functions to access values such as numbers, frequencies etc.,
/// which are common to different subsystems.
module IO =
    /// Check the model number of the instrument matches one that this software is known to work with.
    let private checkModel (N5172B instrument) = async {
        // not checked - that's not our job in the initialisation bit
        let! identity = SCPI.Query.identity instrument
        match identity.Model with
        | "N1572B" -> ()
        | model    ->
            sprintf "Unexpected RF source model number: %s." model
            |> UnexpectedReplyException
            |> raise }
    
    /// Extract the SCPI instrument from the RfSource handle
    let scpiInstrument (N5172B instrument) = instrument

    /// Post a key to the instrument, then check the error queue afterwards.
    let post key (N5172B instrument) = SCPI.Checked.Set.key key instrument

    /// Set a key to a value, then check the error queue after.
    let set<'In> key (value : 'In) (N5172B instrument) = SCPI.Checked.Set.value key value instrument
    /// Write a sequency of values
    let setSeq<'In> key (values : seq<'In>) (N5172B instrument) =
        SCPI.Checked.Set.value key (String.csvSeqString SCPI.format values) instrument

    /// Query a key for a value, then check the error queue after.
    let query (parser : string -> 'Out) key (N5172B instrument) =
        SCPI.Checked.Query.Key.parsed parser key instrument
    /// Query a key and value for a value, then check the error queue.
    let queryValue<'In, 'Out> (parser : string -> 'Out) key (value : 'In) (N5172B instrument) =
        SCPI.Checked.Query.Value.parsed parser key value instrument
    /// Query a key for a CSV sequence of values, each of which is interpreted by the parser command.
    let querySeq (parser : string -> 'Out) key (N5172B instrument) =
        SCPI.Checked.Query.Key.parsed (String.parseCsvSeq parser) key instrument

    /// The key for setting the units of power.
    let [<Literal>] private powerUnitKey = ":UNIT:POW"
    /// The base for querying an amplitude from the machine.
    let private amplitudeBase parser key rfSource = async {
        // get the currently set power unit
        let! unit = query string powerUnitKey rfSource
        do! set powerUnitKey "DBM" rfSource
        let! result = query parser key rfSource
        do! set powerUnitKey unit rfSource
        return result }

    /// Safely query an amplitude from the machine, setting the power units to be
    /// in the correct format before, and returning them to their previous settings
    /// afterwards.
    let queryAmplitude key rfSource = amplitudeBase Parse.amplitude key rfSource
    /// Safely query an amplitude sequence from the machine, setting the power units to be
    /// in the correct format before, and returning them to their previous settings
    /// afterwards.
    let queryAmplitudeSeq key rfSource = amplitudeBase (String.parseCsvSeq Parse.amplitude) key rfSource

    /// Perform initialisation checks on the machine to ensure the driver software will work with it.
    /// Throws SCPI.InvalidResponseException if the machine responds with an invalid string over SCPI,
    /// UnexpectedReplyException if the model does not match, or InstrumentErrorException if there are
    /// errors in the queue.
    let initialise rfSource = async {
        do! checkModel rfSource
        do! SCPI.Checked.Query.errors (scpiInstrument rfSource) }

    /// Return control back to the front panel of the instrument.
    let localControl instrument = post ":SYSTEM:COMM:GTLocal" instrument

    /// Connect to an Agilent E8257D over TCPIP Visa, and perform the initialisation checks.
    let connect visaTcpipAddress timeout = async {
        let instrument = Visa.openTcpipInstrument visaTcpipAddress timeout None
        let rfSource = instrument :> SCPI.IScpiInstrument |> N5172B
        do! initialise rfSource
        return rfSource }

    /// Disconnect from an Agilent E8257D, returning control back to the front panel.
    let disconnect rfSource = async {
        do! localControl rfSource
        do! Visa.closeInstrument (scpiInstrument rfSource :?> Visa.Instrument) }
