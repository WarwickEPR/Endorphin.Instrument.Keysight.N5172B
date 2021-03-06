// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

module Power =
    module Control =
        /// Key to control the ALC circuitry.
        /// Command reference p.77.
        let private alcKey = ":POWER:ALC"
        /// Set the state of the ALC circuitry.
        let setAlcState = fun x -> IO.set<OnOffState> alcKey x
        /// Query the state of the ALC circuitry.
        let queryAlcState = fun x -> IO.query Parse.onOffState alcKey x