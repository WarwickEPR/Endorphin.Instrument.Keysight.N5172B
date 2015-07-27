﻿namespace Endorphin.Instrument.Keysight

open ExtCore.Control
open Endorphin.Core.StringUtils
open Endorphin.Core.NationalInstruments
open System.Text

/// Common functions to set/query values of a VISA Keysight instrument.
/// Includes functions to access values such as numbers, frequencies etc.,
/// which are common to different subsystems.
[<RequireQualifiedAccess>]
module internal IO =
    /// Directly post a command to an RfSource.
    let postCommand command (RfSource rfSource) = Visa.writeString rfSource command

    /// Query an RfSource for the value of a particular key, and parse the response into
    /// the internal representation.
    let internal queryValue parseFunc key (RfSource rfSource) = asyncChoice {
        let! response = sprintf "%s?" key |> Visa.queryString rfSource
        return parseFunc response }

    /// Query an RfSource for the value of a particular key, then try to parse the response
    /// into the internal representation.  If it fails, then return an error message.
    let internal tryQueryValue (tryParseFunc : string -> Choice<'T, string>) key rfSource = asyncChoice {
        let! response = queryValue id key rfSource
        return! tryParseFunc response }

    [<AutoOpen>]
    module Error =
        /// Parse an error message into an error code and the associated message.
        let internal parseError (str : string) =
            let parts = str.Split [|','|]
            if Array.length parts <> 2 then failwithf "Unexpected error string: %s." str
        
            match parts.[0] with
            | ParseInteger code -> { Code = code ; Message = parts.[1] }
            | _                 -> failwithf "Unexpected error code string: %s." parts.[0]

        /// Format an error type nicely as a string.
        let internal errorString error = sprintf "%d: %s" error.Code error.Message

        /// Given a key to specify which error, query the machine for the matching error
        /// and parse the result.
        let queryError = queryValue parseError

        /// Key to find the next error in the machine's queue.
        /// Command reference p.182.
        let private nextErrorInQueueKey = ":SYSTEM:ERROR"
        /// Query the machine for the next error message in the error queue and parse the
        /// result.
        let queryNextErrorInQueue = queryError nextErrorInQueueKey

        /// Loop through the error queue of the machine, creating a sequence of all the listed
        /// errors until the queue is empty.
        let queryErrorQueue rfSource = 
            let rec errorQueueLoop errorList = asyncChoice {
                let! nextError = queryNextErrorInQueue rfSource
                if nextError.Code <> 0 then return! errorQueueLoop (nextError :: errorList)
                else return Seq.ofList <| List.rev errorList }
            errorQueueLoop List.empty

        /// Check if the machine's error queue has any messages in it.
        let internal checkErrorQueueIsEmpty errors =
            if Seq.length errors <> 0 then
                errors
                |> Seq.map errorString
                |> String.concat "\n" 
                |> fail 
            else succeed ()

    /// Set a quantity on the machine to a certain value by covnerting an internal
    /// representation to a machine representation, using the given valueMap.
    let internal setValue valueMap key (RfSource rfSource) value = asyncChoice {
        sprintf "%s %s" key (valueMap value) |> Visa.writeString rfSource
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }

    /// Set a quantity on the machine to a certain value by covnerting an internal
    /// representation to a machine representation, using the given valueMap.  This request
    /// is sent as an ASCII string, rather than a UTF-8 string - it's more difficult to create
    /// and manipulate these values, but there is no risk of errant data causing problems
    /// when encoded to UTF-8.
    let internal setBytesValue valueMap (key : string) (RfSource rfSource) value = asyncChoice {
        let bytesKey = Encoding.ASCII.GetBytes key
        Array.concat [bytesKey; " "B; (valueMap value); "\n"B] |> Visa.writeBytes rfSource
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }

    /// Write a key without a value to the machine.  Useful for "delete all" style functions.
    let internal writeKey key (RfSource rfSource) = asyncChoice {
        postCommand key (RfSource rfSource)
        let! errors = queryErrorQueue (RfSource rfSource)
        do! checkErrorQueueIsEmpty errors }
 
    /// Functions related to identifying the connected machine.
    module Identify =
        /// Attempt to parse a device ID string into an internal representation of a
        /// device ID.
        let internal tryParseDeviceId (str : string) =
            let trimWhiteSpace (str : string) = str.TrimStart([|' '|]).TrimEnd([|' '|])
            let parts = str.Split [|','|]
            if Array.length parts <> 4 then
                fail <| sprintf "Unexpected device ID string: %s." str
            else
                succeed <| {
                Manufacturer = parts.[0] |> trimWhiteSpace
                ModelNumber  = parts.[1] |> trimWhiteSpace
                SerialNumber = parts.[2] |> trimWhiteSpace
                Version      = parts.[3] |> trimWhiteSpace }

        /// Get an internal representation of a device ID from a string, and raise an exception
        /// if the string is not in the correct format.
        let internal parseDeviceId str =
            match tryParseDeviceId str with
            | Success id    -> id
            | Failure error -> failwith error

        /// Key needed to query the identity of any SCPI device.
        let private identityKey = "*IDN"
        /// Query the identity of the given device, and raise an exception if the returned
        /// identity string is not in the expected format.
        let queryIdentity = queryValue parseDeviceId identityKey
        /// Attempt to query the identity of the given device, wrapping up a failure inside a
        /// choice failure.
        let tryQueryIdentity = tryQueryValue tryParseDeviceId identityKey

        /// Check that the model number of a machine is known by the program.
        let private checkModelNumber = function
            | "N5172B" -> succeed N5172B
            | model    -> fail <| sprintf "Unexpected RF source model number: %s." model

        /// Get the model number of the given RfSource, and raise an exception if this model
        /// is not known to the program.
        let identity rfSource = asyncChoice {
            let! identity = tryQueryIdentity rfSource
            return checkModelNumber (identity.ModelNumber) }

    /// Functions for connecting and disconnecting from instruments.
    [<AutoOpen>]
    module Connect =
        /// Open an instrument for communication at the given VISA address, with a specified
        /// timeout in milliseconds.
        let openInstrument visaAddress timeout = asyncChoice {
            let visaInstrument = Visa.openInstrument visaAddress timeout
            let rfSource = RfSource <| visaInstrument
            let! _ = Identify.identity rfSource
            let! _ = Error.queryErrorQueue rfSource // clear the error queue before doing anything
            return rfSource }

        /// Close a given RfSource.
        let closeInstrument (RfSource rfSource) = Visa.closeInstrument rfSource

    /// Set the quantity represented by the given key to have the integer value given.
    let setInt = setValue (fun (i : int) -> i.ToString())
    /// Query the given key for a plain integer value.
    let queryInt = queryValue int

    /// Set the frequency represented by the given key to have the given value.
    let setFrequency = setValue frequencyString
    /// Query the given key for a frequency value.
    let queryFrequency = queryValue parseFrequencyInHz

    /// Set a sequence of frequency values at the given key.
    let setFrequencySeq key = setValue (csvSeqString frequencyString) key
    /// Query a sequence of frequencies, returning the values in Hz.
    let queryFrequencySeq = queryValue (parseCsvSeq parseFrequencyInHz)

    /// Set the amplitude of the given key to have the given value.
    let setAmplitude = setValue amplitudeString
    // TODO: Handle other units?
    /// Query the amplitude at the given key.  Currently only supports returning the value as
    /// dBm, rather than as a ratio.
    let queryAmplitude key (RfSource rfSource) = asyncChoice {
        // Leaves units in original state
        let! powerUnit = ":UNIT:POW?" |> Visa.queryString rfSource
        let! response = sprintf ":UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryString rfSource
        return parseAmplitudeInDbm response }

    /// Set a sequence of amplitude values.
    let setAmplitudeSeq key = setValue (csvSeqString amplitudeString) key 
    /// Query a sequence of amplitudes, returning the values in dBm.
    let queryAmplitudeSeq key (RfSource rfSource) = asyncChoice {
        let! powerUnit = ":UNIT:POW?" |> Visa.queryString rfSource
        let! response = sprintf "UNIT:POW DBM; %s?; :UNIT:POW %s" key powerUnit |> Visa.queryString rfSource
        return parseCsvSeq parseAmplitudeInDbm <| response }

    /// Set the duration of the given key to have the given value in seconds.
    let setDuration = setValue durationString 
    /// Query the given key for a duration in seconds.
    let queryDuration = queryValue parseDurationInSec

    /// Set a sequence of durations to have the given values.
    let setDurationSeq key = setValue (csvSeqString durationString) key
    /// Query a sequence of durations, returning the values in seconds.
    let queryDurationSeq = queryValue (parseCsvSeq parseDurationInSec)

    /// Set the phase of the given key.
    let setPhase = setValue phaseString
    /// Query the given key for a phase in radians.
    let queryPhase = queryValue parsePhaseInRad

    /// Set the given key to the given on/off state.
    let setOnOffState = setValue onOffStateString
    /// Query the given key for an on/off state.
    let queryOnOffState = queryValue parseOnOffState

    /// Set the given key to have the given automatic/manual state.
    let setAutoManualState = setValue autoManualStateString
    /// Query the given key for an automatic/manual state.
    let queryAutoManualState = queryValue parseAutoManualState

    /// Set the given key to have the given direction.
    let setDirection = setValue directionString
    /// Query the given key for a direction.
    let queryDirection = queryValue parseDirection

    /// Set the given key to have the given percentage.
    let setPercentage = setValue percentageString
    /// Query the given key for a percentage value.
    let queryPercentage = queryValue parsePercentage

    /// Set the given key to have the given decibel ratio.
    let setDecibelRatio = setValue decibelRatioString
    /// Query the given key for a decibel ratio.
    let queryDecibelRatio = queryValue parseDecibelRatio
    
    /// Set the given key to have the given polarity.
    let setPolarity = setValue polarityString
    /// Query the given key for a polarity.
    let queryPolarity = queryValue parsePolarity