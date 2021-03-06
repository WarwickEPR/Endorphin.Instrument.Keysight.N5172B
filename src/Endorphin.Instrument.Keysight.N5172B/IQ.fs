// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

module IQ =
    module Control =
        /// Key to control the state of the IQ modulation.
        /// Command reference p.39.
        let private iqModulationKey = ":DM:STATE"
        /// Set the state of the IQ modulation.
        let setIqModulation = fun x -> IO.set<OnOffState> iqModulationKey x
        /// Query the state of the IQ modulation.
        let queryIqModulation = fun x -> IO.query Parse.onOffState iqModulationKey x