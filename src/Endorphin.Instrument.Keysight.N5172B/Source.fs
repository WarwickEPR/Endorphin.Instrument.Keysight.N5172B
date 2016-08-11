// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core

module Source =
    module Control =
        // Keys are e.g. :AM1:INTERNAL:FUNCTION1:SHAPE
        //           or  :FM2:EXT1:COUPLING
        //           or  :FUNCTION1:SHAPE
        // Treated as :AM1:INTERNAL + :FUNCTION1 + :SHAPE
        //         or :FM2 + :EXT1 + :COUPLING
        //         or "" + "FUNCTION1 + :SHAPE
        // Sources can be used directly (low frequency oscillator) or by modulations

        let private sourceString (src : SourceProvider) = SCPI.format src
        /// Concatenate the key, command subsystem prefix and source into a machine-readable
        /// command string.
        let private sourceKey key prefix src =
            sprintf "%s:%s%s" prefix (sourceString src) key

        module Function =
            /// Key to create a function shape.
            let private shapeKey = ":SHAPE"
            /// Key to create a function in the ramp shape.
            let private rampKey =  ":SHAPE:RAMP"
            /// Query the shape of the machine, given the correct source and subsystem.
            let internal queryShape prefix fg str  instrument=
                let getRamp = async {
                    let rkey = sourceKey rampKey prefix fg
                    let! polarity = IO.query Parse.polarity rkey instrument
                    return Ramp polarity }
                let key = sourceKey shapeKey prefix fg
                async {
                    let! shape = IO.query String.toUpper key instrument
                    match shape with
                    | "SINE"             -> return Sine
                    | "TRI" | "TRIANGLE" -> return Triangle
                    | "SQU" | "SQUARE"   -> return Square
                    | "RAMP"             -> return! getRamp
                    | _                  -> return raise << UnexpectedReplyException
                                                   <| sprintf "Unexpected function shape type string: %s" str }

            /// Set the shape of the function generator.
            let internal setShape prefix fg (shape : FunctionShape) instrument =
                let key = sourceKey shapeKey prefix fg
                let rkey = sourceKey rampKey prefix fg
                async {
                    do! IO.set<FunctionShape> key shape instrument
                    match shape with
                    | Ramp polarity -> do! IO.set<Polarity> rkey polarity instrument
                    | _ -> () }

            /// Key for setting frequencies of the function generator.
            let private frequencyKey = ":FREQUENCY"
            /// Set the frequency of the function generator.
            let internal setFrequency prefix fg = IO.set<Frequency> (sourceKey frequencyKey prefix fg)
            /// Query the frequency of the function generator.
            let internal queryFrequency prefix fg = IO.query Parse.frequency (sourceKey frequencyKey prefix fg)

        module External =
            /// Key for the coupling system.
            let private couplingKey = ":COUPLING"
            /// Set the coupling of an external source.
            let internal setCoupling prefix src = IO.set<Coupling> (sourceKey couplingKey prefix src)
            /// Query the coupling of an external source.
            let internal queryCoupling prefix src = IO.query Parse.coupling (sourceKey couplingKey prefix src)

            /// Key for the impedance system.
            let private impedanceKey = ":IMPEDANCE"
            /// Set the impedance of an external source.
            let internal setImpedance prefix src = IO.set<Impedance> (sourceKey impedanceKey prefix src)
            /// Query the impedance of an external source.
            let internal queryImpedance prefix src = IO.query Parse.impedance (sourceKey impedanceKey prefix src)

    module Apply =
        open Control

        /// Apply the given source configurations to the machine.
        let internal setup prefix source instrument = async {
            let sourceProvider = sourceProvider source
            match source with
            | ExternalSource (_, settings) ->
                do! External.setCoupling prefix sourceProvider settings.Coupling instrument
                do! External.setImpedance prefix sourceProvider settings.Impedance instrument
            | InternalSource (_, settings) ->
                do! Function.setShape prefix sourceProvider settings.Shape instrument
                do! Function.setFrequency prefix sourceProvider settings.Frequency instrument }
