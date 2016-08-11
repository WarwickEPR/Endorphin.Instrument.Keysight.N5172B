// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

/// Command set of the Keysight RF instrument.
/// Implements functions to modify & query configuration.
/// Organised by subsystem mirroring the Keysight configuration.
module RfSource =
    /// Apply a set of settings to the given RfSource machine.
    let applySettings  settings instrument = async {
        match settings.Sweep with
        | NoSweep (frequency,amplitude) ->
            do! setCarrierFrequency frequency instrument
            do! setCarrierAmplitude amplitude instrument
        | StepSweep sweep ->
            do! Sweep.Apply.stepSweep sweep instrument
        do! Modulation.Apply.modulationSettings settings.Modulation instrument }
