// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

module internal Filename =
     /// String of the folder location for waveforms.
     let waveformFolder = Some "WFM1:"
     /// String of the folder location for markers.
     let markerFolder   = Some "MKR1:"
     /// String of the folder location for sequences.
     let sequenceFolder = Some "SEQ:"
     /// String of the folder location for list files.
     let listFolder = None

     /// Build up a full file name string for storing a file.
     let string folder name =
         match folder with
         | Some f -> String.concat "" ["\""; f; name; "\""]
         | None -> String.concat "" ["\""; name; "\""]

     /// Total filename string for a waveform file.
     let waveform name = string waveformFolder name
     /// Total filename string for a markers file.
     let marker name = string markerFolder name
     /// Total filename string for a sequence file.
     let sequence name = string sequenceFolder name

     /// Get the string representation of a waveform ID.
     let waveformId : _ -> string = function
         | SegmentId  s -> s
         | SequenceId s -> s

     /// Get the string representations of the relevant folders and the ids of a StoredWaveform.
     let storedWaveformFolderAndId (StoredWaveform id) =
         match id with
         | SegmentId  s -> (waveformFolder, s)
         | SequenceId s -> (sequenceFolder, s)

     /// Get the string representation of a StoredWaveform.
     let storedWaveformId (StoredWaveform id) = waveformId id

     /// Get the full file name of a waveform ID.
     let waveformIdFilename = function
         | SegmentId  s -> waveform s
         | SequenceId s -> sequence s

     /// Get the full file name of a waveform file from the short name stored in the
     /// StoredWaveform.  For example, if it is a segment, and the name is "test", then this
     /// function returns "\"WFM1:test\""B.
     let storedWaveformFilename (StoredWaveform id) = waveformIdFilename id