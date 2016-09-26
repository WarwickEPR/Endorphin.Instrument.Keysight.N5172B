// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core
open FSharp.Data.UnitSystems.SI.UnitSymbols

/// Functions for controlling the writing of experiments using the dual ARB system on the machine.
module Control =
    /// Command to write file to volatile memory.
    /// Command reference p.133, p.151. p.133 is technically for the ":MEM:DATA" command rather
    /// than the ":MMEM:DATA" command, but they are identical, and the former has more
    /// information.
    let private storeDataKey = ":MMEM:DATA"

    /// Functions for controlling the storage, querying and playback of waveforms onto the machine.
    module Waveform =
        /// Key to select a segment or a sequence from the machine
        /// Command reference p.355.
        let private selectArbFileKey = ":RAD:ARB:WAV"
        /// Select a waveform on the machine.
        let select = fun x -> IO.set<StoredWaveform> selectArbFileKey x

        /// Set the header file of the given waveform to have the currently set ARB settings, except
        /// also with the passed
        let private setHeaderArbClockFrequency frequency waveform instrument = async {
            do! select waveform instrument
            do! ARB.setClock frequency instrument
            do! ARB.setHeaderFile instrument }

        /// Functions for controlling waveforms which are explicitly known to be segments rather than the
        /// generic type.  This is where the waveform abstraction fails for speed or space reasons!
        module Segment =
            /// Create an internal stored segment representation.
            let private idToStoredWaveform = SegmentId >> StoredWaveform

            /// Store the three files associated with any segment in the machine's volatile memory,
            /// using the filename given as the id. Returns a StoredSegment type representing the
            /// data stored.
            let internal store (id, segment) rfSource = async {
                let encoded = ARB.Encode.segment id segment
                let commands =
                    SCPI.String.concat [
                        SCPI.String.Set.value storeDataKey (ARB.Encode.waveformDataString encoded)
                        SCPI.String.Set.value storeDataKey (ARB.Encode.markersDataString  encoded) ]
                do! SCPI.Checked.Set.verbatim commands (IO.scpiInstrument rfSource)
                let stored = idToStoredWaveform id
                do! setHeaderArbClockFrequency ARB.defaultClockFrequency stored rfSource
                return stored }

            /// Store a sequence of segments and their ids into the volatile memory of the machine.
            /// Returns an array of StoredSegments.
            let internal storeSeq segments instrument =
                Seq.map (fun x -> store x instrument) segments
                |> Async.Parallel

            /// Query a waveform file of a segment for its datablock.
            let private queryWaveformFile id instrument = async {
                let! data = SCPI.Checked.Query.Value.raw storeDataKey (Filename.waveform id) instrument
                return ARB.Decode.parseWaveformFile data }

            /// Query a marker file of a segment for its datablock.
            let private queryMarkerFile id instrument = async {
                let! data = SCPI.Checked.Query.Value.raw storeDataKey (Filename.marker id) instrument
                return ARB.Decode.parseMarkerFile data }

            /// Query a pair of segment files (waveform and markers) for their segment.
            let internal query id instrument = async {
                let! (i, q) = queryWaveformFile id instrument
                let! markers = queryMarkerFile id instrument
                let samples = Array.map3 ARB.Decode.parseSample i q markers
                let rec loop acc lastId accI = function
                    | i when i = Array.length samples ->
                        Segment (id, { SegmentSamples = acc; SegmentLength = uint16 <| Array.length samples })
                    | i ->
                        let curId = Sample.hash samples.[i]
                        if curId = lastId then
                            acc.[accI] <- (fst acc.[accI], 1u + snd acc.[accI])
                            loop acc lastId accI (i + 1)
                        else
                            let acc' = Array.append acc [| (samples.[i], 1u) |]
                            loop acc' curId (accI + 1) (i + 1)
                return loop [||] "" 0 0 }

            /// Command to delete all waveform, markers and header files stored in the BBG memory
            /// of the machine (the usual place that they're stored in).
            /// Command reference p.152.
            let private deleteAllSegmentsKey = ":MMEM:DEL:WFM"
            /// Delete all the waveform, markers and header files stored in the BBG memory of the
            /// machine (the usual storage location).
            let deleteAll = fun x -> IO.post deleteAllSegmentsKey x

        /// Functions for controlling waveforms which are explicitly known to be sequences rather than the
        /// generic type.  This is where the waveform abstraction fails for speed or space reasons!
        module Sequence =
            /// Convert a string id into a StoredWaveform representing a sequence.
            let private idToStoredWaveform = SequenceId >> StoredWaveform

            /// Key to store sequences to the machine.
            /// Command reference p.345.
            let private storeSequenceKey = ":RAD:ARB:SEQ"
            /// Write a sequence file to the machine and returns the stored sequence type.
            let internal store (id, sequence) instrument = async {
                do! IO.set storeSequenceKey (ARB.Encode.sequence id sequence) instrument
                let stored = idToStoredWaveform id
                do! setHeaderArbClockFrequency ARB.defaultClockFrequency stored instrument
                return stored }

            /// Write a sequence of sequences files to the machine, and return an array of the stored
            /// sequence type.
            let internal storeSeq sequences instrument =
                Seq.map (fun x -> store x instrument) sequences
                |> Async.Parallel

            /// Command to delete all sequence files stored in the internal memory of the machine
            /// (the usual storage location).
            /// Command reference p.146. Uses ":MEM" rather than ":MMEM" for some reason.
            let private deleteAllSequencesKey = ":MEM:DEL:SEQ"
            /// Delete all the sequence files stored in the internal memory of the machine (the usual
            /// storage location).
            let deleteAll = fun x -> IO.post deleteAllSequencesKey x

        // module Waveform = (continued base level)

        /// Store a waveform onto the machine, but don't begin playing it yet.
        let store waveform instrument =
            match waveform with
            | Segment (id, data)  -> Segment.store  (id, data) instrument
            | Sequence (id, data) -> Sequence.store (id, data) instrument

        /// Store a sequence of waveforms onto the machine.
        let storeSeq instrument (sequence : Waveform seq) = async {
            let arr = Array.zeroCreate<StoredWaveform> (Seq.length sequence)
            let mutable i = 0
            for element in sequence do
                let! head = store element instrument
                arr.[i] <- head
                i <- i + 1
            return arr }

        /// Command to delete any file on the machine by name.  If a waveform file is passed,
        /// any associated markers and header files are deleted alongside it.
        /// Command reference p.152.
        let private deleteFileKey = ":MMEM:DEL:NAME"
        /// Delete a waveform from the machine's data storage.  Includes deleting the waveform
        /// file, the markers file and the headers file (if present).
        let delete = fun x -> IO.set<StoredWaveform> deleteFileKey x
        /// Delete all stored waveforms on the machine.
        let deleteAll instrument = async {
            do! Segment.deleteAll  instrument
            do! Sequence.deleteAll instrument }

        /// Begin playing a waveform stored on the instrument.
        let playStored stored instrument = async {
            do! select stored instrument
            do! ARB.turnOn instrument }

        /// Store a segment file on to the machine, then begin playing it back as soon as possible.
        let play waveform instrument = async {
            let! stored = store waveform instrument
            do! playStored stored instrument
            return stored }

    /// Functions for controlling the writing, reading and playing of experiments on the machine.
    module Experiment =
        open Experiment.Translate

        /// Store an experiment onto the machine as a set of necessary sequences and samples.
        /// Turns off the ARB before writing, because the machine doesn't seem to like doing both
        /// at once.  This is just stored, not played yet.
        let store experiment instrument = async {
            do! ARB.turnOff instrument
            let encoded = toEncodedExperiment experiment
            let! storedSegments   = Waveform.Segment.storeSeq encoded.Segments instrument
            let! storedSequences  = Waveform.Sequence.storeSeq encoded.Sequences  instrument
            let! storedPoints     = Waveform.Sequence.storeSeq encoded.Points     instrument
            let! storedExperiment = Waveform.Sequence.store    encoded.Experiment instrument
            return {
                StoredExperiment = storedExperiment
                StoredPoints     = storedPoints
                StoredWaveforms  = Array.append storedSegments storedSequences
                RfBlankRoute     = encoded.Metadata.RfBlankMarker } }

        /// Play a previously stored experiment through the dual ARB system.
        let playStored experiment instrument = Waveform.playStored experiment.StoredExperiment instrument

        /// Store an experiment, and then immediately begin playback on the dual ARB system.
        let play experiment instrument = async {
            let! stored = store experiment instrument
            do! playStored stored instrument }
