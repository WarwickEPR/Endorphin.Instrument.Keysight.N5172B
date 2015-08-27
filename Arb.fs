﻿namespace Endorphin.Instrument.Keysight

open System
open Hashing
open ExtCore.Control
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

module ARB =
    /// The shortest length of time a pulse can be, measured in seconds.
    let shortestPulseDuration = (2.0e-8<s>)/3.0
    /// The default clock rate for the dual ARB system.
    let defaultArbClockFrequency = FrequencyInHz 150.0e6<Hz>

    /// Key for use with the dual ARB clock frequency.
    /// Command reference p.344.
    let private dualArbClockKey = ":RADIO:ARB:SCLOCK:RATE"
    /// Set the dual ARB clock frequency to the value specified.
    let setDualArbClock = IO.setFrequency dualArbClockKey
    /// Query the current value of the dual ARB clock frequency.
    let queryDualArbClock = IO.queryFrequency dualArbClockKey

    /// Key for saving header files of waveform segments in the dual ARB system.
    let private dualArbSaveHeaderKey = ":RADIO:ARB:HEADER:SAVE"
    /// Save the current dual ARB settings to the header file of the currently selected waveform.
    let setHeaderFile = IO.writeKey dualArbSaveHeaderKey

    [<RequireQualifiedAccess>]
    module Trigger =
        /// Key for the type of the mode of the dual ARB system's trigger.
        /// Command reference p.347.
        let private modeTypeKey = ":RADIO:ARB:TRIGGER:TYPE"

        /// Key for the type of the source of the dual ARB system's trigger.
        /// Command reference p.352.
        let private sourceTypeKey = ":RADIO:ARB:TRIGGER:SOURCE"

        /// Key for the mode of the continuous trigger of the dual ARB system.
        /// Command reference p.349.
        let private continuousModeKey = ":RADIO:ARB:TRIGGER:TYPE:CONTINUOUS"

        /// Key for the number of repeats in the dual ARB single trigger mode.
        /// Command reference p.351.
        let private singleRepeatsKey = ":RADIO:ARB:TRIGGER:TYPE:SINGLE:REPEAT"

        /// Key for the retrigger mode of the dual ARB system.
        /// Command reference p.343.
        let private retriggerModeKey = ":RADIO:ARB:RETRIGGER"

        /// Key for the polarity of the gate-type trigger of the dual ARB system.
        /// Command reference p.350.
        let private gatePolarityKey = ":RADIO:ARB:TRIGGER:TYPE:GATE"

        /// Key for the mode of the segment advance type trigger of the dual ARB system.
        /// Command reference p.350.
        let private segmentAdvanceModeKey = ":RADIO:ARB:TRIGGER:TYPE:SADVANCE"

        /// Key for the duration of the delay between receiving an external trigger and beginning
        /// playback of the waveform.
        /// Command reference p.353.
        let private delayKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:DELAY"

        /// Key for the state of the delay in an external trigger source for the dual ARB
        /// system.
        /// Command reference p.353.
        let private delayStateKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:DELAY:STATE"

        /// Key for the polarity of the external trigger source for all trigger modes except
        /// gated (which has its own polarity).
        /// Command reference p.354.
        let private polarityKey = ":RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:SLOPE"

        /// Key for the physical location of an external trigger source of the dual ARB system.
        /// Command reference p.354.
        let private sourceLocationKey = ":SOURCE:RADIO:ARB:TRIGGER:SOURCE:EXTERNAL:SOURCE"
        // This key has 3 of the word "SOURCE" in it, and all 3 are optional! But I put them in
        // because I needed entertainment.

        /// Get a machine-readable string representation of the ARB trigger mode type.
        let private modeTypeString = function
            | ArbContinuous _ -> "CONT"
            | ArbSingle _ -> "SING"
            | ArbGate _ -> "GATE"
            | ArbSegmentAdvance _ -> "SADV"

        /// Get a machine-readable string representation of the ARB trigger source type.
        let private sourceTypeString = function
            | ArbKey -> "KEY"
            | ArbBus -> "BUS"
            | ArbExternal _ -> "EXT"

        /// Convert an internal representation of the continuous type mode of the dual ARB triggering
        /// system into a machine representation.
        let private continuousModeString = function
            | ArbContinuousFree -> "FREE"
            | ArbContinuousTrigger -> "TRIGGER"
            | ArbContinuousReset -> "RESET"

        /// Convert a machine representation of the continuous type mode of the dual ARB triggering
        /// system into an internal representation.
        let private parseContinuousMode str =
            match String.toUpper str with
            | "FREE" -> ArbContinuousFree
            | "TRIG" | "TRIGGER" -> ArbContinuousTrigger
            | "RES" | "RESET" -> ArbContinuousReset
            | _ -> failwithf "Unexpected ARB continuous mode trigger type string: %s" str

        /// Convert an internal representation of the single trigger retrigger mode of the dual
        /// ARB trigger system into a machine representation.
        let private retriggerModeString = function
            | NoRetrigger -> "OFF"
            | BufferedRetrigger -> "ON"
            | RestartRetrigger -> "IMMEDIATE"

        /// Covnert a machine representaiton of the single trigger retrigger mode of the dual ARB
        /// trigger system into an internal representation.
        let private parseRetriggerMode str =
            match String.toUpper str with
            | "ON"  | "1" -> BufferedRetrigger
            | "OFF" | "0" -> NoRetrigger
            | "IMM" | "IMMEDIATE" -> RestartRetrigger
            | _ -> failwithf "Unexpected dual ARB retrigger string: %s" str

        /// Convert an internal representation of the segment advance type mode of the dual ARB
        /// triggering system into a machine representation.
        let private segmentAdvanceModeString = function
            | ArbSegmentAdvanceSingle -> "SINGLE"
            | ArbSegmentAdvanceContinuous -> "CONTINUOUS"

        /// Convert a machine representation of the segment advance type mode of the dual ARB
        /// triggering system into a machine representation.
        let private parseSegmentAdvanceMode str =
            match String.toUpper str with
            | "SING" | "SINGLE" -> ArbSegmentAdvanceSingle
            | "CONT" | "CONTINUOUS" -> ArbSegmentAdvanceContinuous
            | _ -> failwithf "Unexpected ARB segment advance mode trigger type string: %s" str

        /// Convert an internal representation of the physical location of an external dual ARB
        /// trigger into a machine representation.
        let private externalConnectorString = function
            | ArbBnc -> "EPT1"
            | ArbAux -> "EPT2"

        /// Convert a machine representation of the physical location of an external dual ARB
        /// trigger into an internal representation.
        let private parseExternalConnector str =
            match String.toUpper str with
            | "EPT1" | "EPTRIGGER1" -> ArbBnc
            | "EPT2" | "EPTRIGGER2" -> ArbAux
            | _ -> failwithf "Unexpected ARB external trigger source location string: %s" str

        /// Set the type of the ARB trigger to the given type.
        let private setModeType = IO.setValueString modeTypeString modeTypeKey

        /// Set the mode of the dual ARB continuous trigger.
        let private setContinuousMode = IO.setValueString continuousModeString continuousModeKey

        /// Set the number of repeats per point in the dual ARB single trigger mode.
        let private setSingleRepeats = IO.setUint16 singleRepeatsKey

        /// Set the retrigger mode of the single trigger setting of the dual ARB.
        let private setRetriggerMode = IO.setValueString retriggerModeString retriggerModeKey

        /// Set the polarity of the gating trigger in the dual ARB system.
        let private setGatePolarity = IO.setLowHighState gatePolarityKey

        /// Set the mode of the segment advance trigger of the dual ARB system.
        let private setSegmentAdvanceMode =
            IO.setValueString segmentAdvanceModeString segmentAdvanceModeKey

        /// Set the dual ARB trigger mode to have the value given.
        let internal setMode instrument mode = asyncChoice {
            do! setModeType instrument mode
            match mode with
            | ArbContinuous mode'     -> do! setContinuousMode instrument mode'
            | ArbSingle (reps, retrigger) ->
                do! setSingleRepeats instrument reps
                do! setRetriggerMode instrument retrigger
            | ArbGate polarity        -> do! setGatePolarity instrument polarity
            | ArbSegmentAdvance mode' -> do! setSegmentAdvanceMode instrument mode' }

        /// Query the currently set value of the dual ARB system triggering.
        let private queryMode instrument = asyncChoice {
            let helper str =
                match String.toUpper str with
                    | "CONT" | "CONTINUOUS" ->
                        IO.queryKeyString parseContinuousMode continuousModeKey instrument
                        |> AsyncChoice.map ArbContinuous
                    | "SING" | "SINGLE" ->
                        let reps = IO.queryUint16 singleRepeatsKey instrument
                        let retrigger = IO.queryKeyString parseRetriggerMode retriggerModeKey instrument
                        AsyncChoice.map2 (fun a b -> ArbSingle (a, b)) reps retrigger
                    | "GATE" ->
                        IO.queryLowHighState gatePolarityKey instrument
                        |> AsyncChoice.map ArbGate
                    | "SADV" | "SADVANCE" ->
                        IO.queryKeyString parseSegmentAdvanceMode segmentAdvanceModeKey instrument
                        |> AsyncChoice.map ArbSegmentAdvance
                    | str -> failwithf "Unexpected ARB trigger type string: %s" str
            let! triggerType = IO.queryKeyString (fun str -> str) modeTypeKey instrument
            return! helper triggerType }

        /// Set the type of the source of the dual ARB triggering system.
        let private setSourceType =
            IO.setValueString sourceTypeString sourceTypeKey

        /// Set the physical location of the external trigger for the dual ARB system.
        let private setSourceConnector =
            IO.setValueString externalConnectorString sourceLocationKey

        /// Set the polarity of the dual ARB external trigger source.
        let private setSourcePolarity instrument polarity = asyncChoice {
            match polarity with
            | Some p -> do! IO.setPolarity polarityKey instrument p
            | None -> () }

        /// Set the delay of the dual ARB trigger system for external trigges.
        let private setSourceDelay instrument delay = asyncChoice {
            match delay with
            | Some d ->
                do! IO.setDuration delayKey instrument d
                do! IO.setOnOffState delayStateKey instrument On
            | None ->
                do! IO.setOnOffState delayStateKey instrument Off }

        /// Set the dual ARB trigger source.
        let internal setSource instrument source = asyncChoice {
            do! setSourceType instrument source
            match source with
            | ArbExternal (connector, polarity, delay) ->
                do! setSourceConnector instrument connector
                do! setSourcePolarity instrument polarity
                do! setSourceDelay instrument delay
            | _ -> () }

        /// Query the source of the dual ARB's triggering system.
        let private querySource instrument mode = asyncChoice {
            let! sourceType = IO.queryKeyString (fun str -> str) sourceTypeKey instrument
            match String.toUpper sourceType with
            | "KEY" -> return ArbKey
            | "BUS" -> return ArbBus
            | "EXT" ->
                let! connector = IO.queryKeyString parseExternalConnector sourceLocationKey instrument
                let! polarity =
                    match mode with
                    | ArbGate _ -> AsyncChoice.liftChoice <| succeed None
                    | _ -> AsyncChoice.map Some <| IO.queryPolarity polarityKey instrument
                let! state = IO.queryOnOffState delayStateKey instrument
                let! delay =
                    match state with
                    | On -> AsyncChoice.map Some <| IO.queryDuration delayKey instrument
                    | Off -> AsyncChoice.liftChoice <| succeed None
                return ArbExternal (connector, polarity, delay)
            | _ -> return! (fail <| sprintf "Unexpected ARB trigger source string: %s" sourceType) }

        /// Completely set the dual ARB system's trigger.
        let set instrument (ArbTrigger (mode, source)) = asyncChoice {
            do! setMode instrument mode
            do! setSource instrument source }

        /// Query the complete settings of the dual ARB's current trigger.
        let query instrument = asyncChoice {
            let! mode = queryMode instrument
            let! source = querySource instrument mode
            return ArbTrigger (mode, source) }

    /// Functions for encoding segments and samples into a writeable form.
    [<AutoOpen>]
    module internal Translate =
        [<AutoOpen>]
        module Encode =
            /// Make a marker byte out of the booleans in an IQ sample.
            let private getMarkerByte (sample : Sample) =
                ((Convert.ToByte sample.Markers.M4) <<< 3) ||| ((Convert.ToByte sample.Markers.M3) <<< 2)
                ||| ((Convert.ToByte sample.Markers.M2) <<< 1) ||| (Convert.ToByte sample.Markers.M1)

            /// Convert a 16-bit integer to an array of bytes in machine order.
            let private toBytes (number : int16) =
                [| byte ((number &&& 0xFF00s) >>> 8); byte (number &&& 0xFFs) |]

            /// Get a four-byte array of the IQ data in the correct endianness.
            let private iqBytes sample =
                let i = toBytes sample.I
                let q = toBytes sample.Q
                [| i.[0]; i.[1]; q.[0]; q.[1] |]

            /// Create a tuple of iq, markers encoded as byte sequences.
            let toEncodedSegmentData (segment : Segment) =
                let sampleCount = int segment.SegmentLength
                let iq = Array.create (sampleCount * 4) 0uy
                let markers = Array.create sampleCount 0uy
                let mutable sampleIndex = 0
                let mutable used = 0u
                let singleIq = Array.create 4 0uy
                let mutable singleMarkers = 0uy
                for i in 0 .. (sampleCount - 1) do
                    let (sample, SampleCount count) = segment.SegmentSamples.[sampleIndex]
                    if used = 0u then
                        singleIq.[0 .. 3] <- iqBytes sample
                        singleMarkers <- getMarkerByte sample
                    iq.[(4 * i) .. (4 * i) + 3] <- singleIq
                    markers.[i]      <- singleMarkers
                    if used = count - 1u then
                        used <- 0u
                        sampleIndex <- sampleIndex + 1
                    else used <- used + 1u
                (iq, markers)

            /// Encode a segment into the necessary byte patterns.
            let private toEncodedSegment segment =
                let (iq, markers) = toEncodedSegmentData segment
                { EncodedIQ = iq
                  EncodedMarkers = markers }

            /// Make the data string, including the '#' character, the digits of length, the length
            /// and the data.
            let private dataString (data : byte []) =
                let length = data.Length
                let digits = length.ToString().Length
                if digits >= 10 then
                    failwith "Can't write 1GB in one go!"
                Array.concat [
                    "#"B
                    Text.Encoding.ASCII.GetBytes(digits.ToString())
                    Text.Encoding.ASCII.GetBytes(length.ToString())
                    data ]

            /// Build up a full string for data storage and location.
            let private dataStorageString (fileName : string) dataString =
                Array.concat [System.Text.Encoding.ASCII.GetBytes fileName; ","B; dataString]

            /// Produce the full data strings necessary for writing the two different files
            /// to the machine, given the encoded segment to extract the data from.  Ignores
            /// the header file, but the only bits we usually care about here are more easily
            /// set by SCPI commands.
            let toEncodedSegmentFiles segment id =
                let encodedSegment = toEncodedSegment segment
                let waveformFilename = waveformFileString id
                let markerFilename   = markerFileString   id
                let waveformDataString = dataString encodedSegment.EncodedIQ
                let markerDataString   = dataString encodedSegment.EncodedMarkers
                { Waveform = dataStorageString  waveformFilename waveformDataString
                  Markers  = dataStorageString  markerFilename   markerDataString }

            /// Get the whole string necessary to write a waveform file to the machine.
            let waveformDataString (encoded : EncodedSegmentFiles) = encoded.Waveform
            /// Get the whole string necessary to write a marker file to the machine.
            let markersDataString (encoded : EncodedSegmentFiles) = encoded.Markers

            /// Make a sequence element into a tuple of the byte array of the full filename
            /// and the ASCII representation of the number of repetitions.
            let private asciiSequenceElement (el, reps) =
                (asciiString <| waveformIdFilename el, asciiString reps)

            /// Encode a sequence element into the form "\"<filename>\",<reps>,<markers>"B.
            let private toEncodedSequenceElement (element : SequenceElement) =
                sprintf "%s,%u,ALL" (waveformIdFilename <| fst element) (snd element)

            /// Convert a sequence into an ASCII string of its elements.
            let private sequenceData (SequenceType sequence) =
                sequence
                |> List.map toEncodedSequenceElement
                |> List.map (sprintf ",%s")
                |> String.concat ""

            /// Encode a whole sequence in an EncodedSequence.
            let sequenceDataString id (sequence : Sequence) =
                sprintf "%s%s" (sequenceFileString id) (sequenceData sequence)

            /// Get a unique representation of a sample as a byte array.
            let sampleToBytes sample =
                let arr = Array.create 5 0uy
                arr.[0 .. 3] <- iqBytes sample
                arr.[4] <- getMarkerByte sample
                arr

            /// Get a unique representation of a segment as a byte array.
            let segmentToBytes segment =
                let length = segment.SegmentLength
                let arr = Array.create (int <| length * 9us) 0uy // 5 bytes per sample, 4 bytes per count
                for i in 0 .. int <| length - 1us do
                    let (sample, SampleCount reps) = segment.SegmentSamples.[i]
                    arr.[(i * 9) + 0 .. (i * 9) + 4] <- sampleToBytes sample
                    arr.[(i * 9) + 5 .. (i * 9) + 8] <- BitConverter.GetBytes reps
                    // endianness doesn't matter here
                arr // return the byte array we just created

            /// Get a unique representation of a sequence as a byte array.
            let sequenceToBytes = sequenceData >> asciiString

        /// Functions for decoding segment and sequence data received from the machine.
        [<AutoOpen>]
        module Decode =
            /// Convert a big-endian array of bytes into the host order.
            let private toHostOrder bytes =
                if BitConverter.IsLittleEndian then
                    bytes |> Array.rev
                else
                    bytes

            /// Decompress the markers back into a 4-tuple of the 4 Boolean markers.
            let private getMarkers markers =
                { M1 = Convert.ToBoolean(markers &&& 0x1uy)
                  M2 = Convert.ToBoolean(markers &&& 0x2uy)
                  M3 = Convert.ToBoolean(markers &&& 0x4uy)
                  M4 = Convert.ToBoolean(markers &&& 0x8uy) }

            /// Decode an encoded sample back into the internal representation of a sample.
            let parseSample i q markers = { I = i; Q = q; Markers = markers }

            /// Get only the interesting bits of the datablock, removing the "#", the number of
            /// digits, and the data length.
            let private stripMetadata (data : byte array) =
                /// datablock is of form "#<digits><length><data>"
                data.[2 .. (Array.length data - 1)]

            /// Parse a waveform file into a tuple of i and q data arrays.
            let parseWaveformFile (data : byte array) =
                let data' = data |> stripMetadata
                let numSamples = (Array.length data') / 4
                let i = Array.create numSamples 0s
                let q = Array.create numSamples 0s
                let rec loop = function
                    | index when index = numSamples -> (i, q)
                    | index ->
                        i.[index] <-
                            BitConverter.ToInt16(toHostOrder data'.[(4 * index)     .. (4 * index + 1)], 0)
                        q.[index] <-
                            BitConverter.ToInt16(toHostOrder data'.[(4 * index + 2) .. (4 * index + 3)], 0)
                        loop (index + 1)
                loop 0

            /// Parse a marker file into an array of markers.
            let parseMarkerFile (data : byte array) =
                let data' = data |> stripMetadata
                let numSamples = (Array.length data')
                let markers = Array.create numSamples Unchecked.defaultof<Markers>
                let rec loop = function
                    | index when index = numSamples -> markers
                    | index ->
                        markers.[index] <- getMarkers data'.[index]
                        loop (index + 1)
                loop 0

#if DEBUG
    [<AutoOpen>]
    module internal Print =
        /// Depth of an indent.
        let indentDepth = 4

        /// Get a string of the indent level.
        let private getIndent indent = String.replicate indent " "

        /// Pretty-print out a sample.
        let printSample (indent : int) sample =
            printf "%s(%6d; %6d; %d%d%d%d)"
                (getIndent indent)
                sample.I
                sample.Q
                (Convert.ToInt32 sample.Markers.M1)
                (Convert.ToInt32 sample.Markers.M2)
                (Convert.ToInt32 sample.Markers.M3)
                (Convert.ToInt32 sample.Markers.M4)

        /// Print out a (Sample * SampleCount) tuple.
        let printSampleCount indent (smp, SampleCount count) =
            printSample indent smp
            printfn " * %d" count

        /// Pretty-print out a segment.
        let printSegment indent segment =
            segment.SegmentSamples
            |> Array.iter (printSampleCount indent)

        /// Pretty print a pending sequence.
        let rec printSequence indent segMap seqMap (SequenceType sequence) =
            let printEl = printSequenceElement indent segMap seqMap
            sequence |> List.iter printEl
        and printSequenceElement indent segMap seqMap (id, reps) =
            match id with
            | SegmentId id ->
                printfn "%s%s * %d" (getIndent indent) id reps
                printSegment (indent + indentDepth) (Map.find id segMap)
            | SequenceId id ->
                printfn "%s%s * %d" (getIndent indent) id reps
                printSequence (indent + indentDepth) segMap seqMap (Map.find id seqMap)
#endif

[<RequireQualifiedAccess>]
module Markers =
    /// A markers record with all markers turned off.
    let empty = { M1 = false; M2 = false; M3 = false; M4 = false }

    /// Set value of the first marker.
    let withMarker1 value markers = { markers with M1 = value }
    /// Set value of the second marker.
    let withMarker2 value markers = { markers with M2 = value }
    /// Set value of the third marker.
    let withMarker3 value markers = { markers with M3 = value }
    /// Set value of the fourth marker.
    let withMarker4 value markers = { markers with M4 = value }

[<RequireQualifiedAccess>]
module Sample =
    /// Basic data form of IQ point.
    let empty = {
        Sample.I = 0s
        Sample.Q = 0s
        Sample.Markers = Markers.empty }

    /// Set value of the I sample.
    let withI value sample = { sample with I = value }
    /// Set value of the Q sample.
    let withQ value sample = { sample with Q = value }

    /// Set value of the first marker.
    let withMarker1 value (sample : Sample) =
        { sample with Markers = Markers.withMarker1 value sample.Markers }
    /// Set value of the second marker.
    let withMarker2 value (sample : Sample) =
        { sample with Markers = Markers.withMarker2 value sample.Markers }
    /// Set value of the third marker.
    let withMarker3 value (sample : Sample) =
        { sample with Markers = Markers.withMarker3 value sample.Markers }
    /// Set value of the fourth marker.
    let withMarker4 value (sample : Sample) =
        { sample with Markers = Markers.withMarker4 value sample.Markers }

    /// Set value of all markers at once.
    let withMarkers markers (sample: Sample) =
        { sample with Markers = markers }

    /// Convert a Phase type into a float value of radians for use in the mathematical functions.
    let private phaseToRadians = function
        // We want IQ to be equal at 0 phase, so rotate phases by pi/4
        | PhaseInRad (angle) -> (angle / 1.0<rad>) + (Math.PI / 4.0)
        | PhaseInDeg (angle) -> (angle * (Math.PI * 2.0 / 360.0) * 1.0<1/deg>) + (Math.PI / 4.0)

    /// The maximum amplitude in arbitrary units that the machine can take for an IQ point amplitude.
    let private maximumMachineAmplitude = Int16.MaxValue

    /// Generate a sample at the given amplitude and phase.  The amplitude is relative to the
    /// maximum amplitude available with the current scaling setting on the machine.
    /// I and Q are equal when phase is 0.
    let withAmplitudeAndPhase relativeAmplitude phase sample =
        let phaseAngle = phaseToRadians phase
        let amplitude = relativeAmplitude * float maximumMachineAmplitude
        sample
        |> withI (int16 (amplitude * Math.Cos phaseAngle))
        |> withQ (int16 (amplitude * Math.Sin phaseAngle))

[<RequireQualifiedAccess>]
module Segment =
    /// An empty segment, ready to have samples added to it.
    let empty = {
        SegmentSamples = Array.empty
        SegmentLength  = 0us }

    /// Minimum length a segment may be for playback on the ARB.
    let minimumLength = 60u

    /// Get the samples associated with a segment.
    let internal samples (segment : Segment) = segment.SegmentSamples

    /// Get the length of a segment.
    let length (segment : Segment) = segment.SegmentLength

    /// Add a sample and a number of repeats to a waveform.
    let add sample count segment = {
        SegmentSamples = Array.append (samples segment) [| (sample, SampleCount <| uint32 count) |]
        SegmentLength  = length segment + count }

    /// Add a sequence of (sample, count) onto a segment.
    let addSeq sequence segment = {
        SegmentSamples =
            sequence
            |> Seq.map (fun (x, y) -> (x, SampleCount y))
            |> Array.ofSeq
            |> Array.append (samples segment)
        SegmentLength =
            sequence
            |> Seq.sumBy snd
            |> uint16
            |> (+) (length segment) }

    /// Complete a segment, creating a waveform to write to the machine.
    let toWaveform segment =
        let id = hexHash ARB.Translate.Encode.segmentToBytes segment
        Segment (id, segment)

[<RequireQualifiedAccess>]
module Sequence =
    /// An empty sequence, ready to have waveforms added to it.
    let empty = SequenceType List.empty

    /// Add a waveform sequence and count to a sequence.
    let add waveform count (SequenceType sequence) =
        let id =
            match waveform with
            | Segment (id, _) -> SegmentId id
            | Sequence (id, _) -> SequenceId id
        SequenceType ((id, count) :: sequence)

    /// Complete a sequence, creating a waveform to write to the machine.
    let toWaveform (SequenceType sequence) =
        let id = hexHash ARB.Translate.Encode.sequenceToBytes (SequenceType sequence)
        Sequence (id, SequenceType (List.rev sequence))