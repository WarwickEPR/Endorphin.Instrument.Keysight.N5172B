// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

[<AutoOpen>]
module internal InternalModel =
    [<AutoOpen>]
    module ARB =
        /// Internal record of an entire recorded segment before being transformed into
        /// machine-readable strings.  Lists are in reverse order for speed.
        type EncodedSegment =
            { EncodedIQ      : byte array
              EncodedMarkers : byte array }

        /// Segment data after it has been encoded, including the filename, lengths and
        /// data indicator '#'. Ready to write to machine as a value in the SCPI functions.
        type EncodedSegmentFiles =
            { Waveform : byte array
              Markers  : byte array }

    [<AutoOpen>]
    module Experiment =
        /// A verified pulse, identical to the regular pulse, but we're sure that (for example)
        /// the number of pulses in each cycle are the same.
        type VerifiedPulse =
            | VerifiedRf of RfPulse
            | VerifiedDelay of DelayPulse
            | VerifiedMarker of MarkerPulse

        /// Metadata about the experiment gathered during verification, for use during the
        /// compilation step.
        type ExperimentMetadata =
            { ExperimentRepetitions : int
              PulseCount            : int
              RfPulseCount          : int
              RfPhaseCount          : int option
              RfBlankMarker         : UserSignalMarker
              ShotRepetitionTime    : uint32
              ShotsPerPoint         : uint16 }

        // No need for type aliases here because there's no other step which uses similar types
        /// A single pulse which can be easily converted into a single segment, for use after the
        /// compilation of the experiment and optimisation phases.
        type StaticPulse =
            | StaticRf      of phase : Phase * duration : uint32
            | StaticDelay   of duration : uint32
            | StaticMarker  of markers : Markers * duration : uint32

        /// An experiment after it has been passed through the user-input verifier.
        type VerifiedExperiment =
            { Pulses   : VerifiedPulse list list // double list because we're going to expand into individual lists
              Metadata : ExperimentMetadata }

        type CompiledExperimentPoint =
            { CompiledData   : (Sample * uint32) list
              CompiledLength : uint32 }

        /// A list of samples and their repetitions, which could be easily written onto the
        /// machine, but likely with a lot of redundancy.
        type CompiledExperiment =
            { ExperimentPoints : CompiledExperimentPoint list
              Metadata         : ExperimentMetadata }

        /// One element of compression - usually analagous to a single point in the experiment,
        /// with one phase and one duration for the pulses.
        type CompressedElement =
            { Element   : SequenceElement
              Segments  : Map<string, Segment>
              Sequences : Map<string, Sequence> }

        /// An experiment inside the compression step.
        type CompressedExperiment =
            { Segments         : Map<string, Segment>
              Sequences        : Map<string, Sequence>
              CompressedPoints : Sequence list
              Metadata         : ExperimentMetadata }

        /// An assembled experiment, ready for storing onto the machine.
        type EncodedExperiment =
            { Segments   : (string * Segment) list
              Sequences  : (string * Sequence) list
              Points     : (string * Sequence) list
              Experiment : (string * Sequence)
              Metadata   : ExperimentMetadata }