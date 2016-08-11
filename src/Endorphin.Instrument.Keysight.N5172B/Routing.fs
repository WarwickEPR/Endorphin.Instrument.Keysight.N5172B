// Copyright (c) University of Warwick. All Rights Reserved. Licensed under the Apache License, Version 2.0. See LICENSE.txt in the project root for license information.

namespace Endorphin.Instrument.Keysight.N5172B

open Endorphin.Core

module Routing =
    /// The default output routing that the machine would use after a *RST command.
    let private defaultOutputRouting = {
        BbTrig1  = RouteMarker2
        BbTrig2  = NoSignal
        Event1   = RouteMarker1
        PatTrig  = NoSignal
        SweepOut = RouteSweepOut
        Trig1    = NoSignal
        Trig2    = RouteSweepTriggerOut }

    /// The default input routing that the machine would use after a *RST command.
    let private defaultInputRouting = {
        PatTrig1 = RoutePatternTrigger
        PatTrig2 = RoutePatternTrigger }

    /// The default internal routing that the machine would use after a *RST command.
    let private defaultInternalRouting = {
        AltAmplitude = NoSignal
        AlcHold = NoSignal
        RfBlank = NoSignal }

    /// Default polarities of the marker channels after a *RST command.
    let private defaultMarkerPolarities = {
        PolarityM1 = Positive
        PolarityM2 = Positive
        PolarityM3 = Positive
        PolarityM4 = Positive }

    /// The default routings for the machine, in use after a *RST command.
    let empty = {
        Output           = defaultOutputRouting
        Input            = defaultInputRouting
        Internal         = defaultInternalRouting
        MarkerPolarities = defaultMarkerPolarities }

    /// If any inputs are set to "value", then unset them.  Otherwise, leave them be.
    let private unsetRequiredInputs value routing =
        let patTrig1 =
            if routing.Input.PatTrig1 = value then NoSignal :> IUserBncSignal
            else routing.Input.PatTrig1
        let patTrig2 =
            if routing.Input.PatTrig2 = value then NoSignal :> IUserBncSignal
            else routing.Input.PatTrig2
        { routing with Input = { PatTrig1 = patTrig1; PatTrig2 = patTrig2 } }

    /// Set the routing of the BBTRIG1 connector. Overwrites any inputs set to come into this BNC.
    let withBasebandTrigger1 value routing =
        let routing' = unsetRequiredInputs RouteBasebandTrigger1 routing
        { routing' with Output = { routing'.Output with BbTrig1 = value } }
    /// Set the routing of the BBTRIG2 connector. Overwrites any inputs set to come into this BNC.
    let withBasebandTrigger2 value routing =
        let routing' = unsetRequiredInputs RouteBasebandTrigger2 routing
        { routing' with Output = { routing'.Output with BbTrig2 = value } }

    /// Set the routing of the EVENT1 connector. Overwrites any inputs set to come into this BNC.
    let withEvent1 value routing =
        let routing' = unsetRequiredInputs RouteEvent1 routing
        { routing' with Output = { routing'.Output with Event1 = value } }

    /// Set the routing of the PATTRIG connector. Overwrites any inputs set to come into this BNC.
    let withPatternTrigger value routing =
        let routing' = unsetRequiredInputs RoutePatternTrigger routing
        { routing' with Output = { routing'.Output with PatTrig = value } }

    /// Set the routing of the SWEEPOUT connector.
    let withSweepOut value routing =
        { routing with Output = { routing.Output with SweepOut = value } }

    /// Set the routing of the TRIG1 connector.
    let withTrigger1 value routing =
        { routing with Output = { routing.Output with Trig1 = value } }
    /// Set the routing of the TRIG2 connector.
    let withTrigger2 value routing =
        { routing with Output = { routing.Output with Trig2 = value } }

    /// Set a given BNC to have no output signal.
    let private unsetBncOutput routing (value : IUserBncSignal) =
        match value with
        | :? NoSignal -> routing
        | :? UserBnc as value ->
            match value with
            | RouteBasebandTrigger1 -> withBasebandTrigger1 NoSignal routing
            | RouteBasebandTrigger2 -> withBasebandTrigger2 NoSignal routing
            | RouteEvent1           -> withEvent1 NoSignal routing
            | RoutePatternTrigger   -> withPatternTrigger NoSignal routing
        | _ -> Parse.failIncorrectType value

    /// Set the routing of the internal signal pattern trigger 1.  This overwrites any output
    /// signal on the given BNC.
    let withPatternTrigger1 value routing =
        let routing' = unsetBncOutput routing value
        { routing' with Input = { routing'.Input with PatTrig1 = value } }

    /// Set the routing of the internal signal pattern trigger 2.  This overwrites any output
    /// signal on the given BNC.
    let withPatternTrigger2 value routing =
        let routing' = unsetBncOutput routing value
        { routing' with Input = { routing'.Input with PatTrig2 = value } }

    /// Set the routing of the alternate amplitude function.
    let withAlternateAmplitude value routing =
        { routing with Internal = { routing.Internal with AltAmplitude = value } }

    /// Set the routing of the ALC hold function. This overwrites the RF blank function.
    let withAlcHold value routing =
        let rfBlank =
            if value = (NoSignal :> IMarkerSignal) then routing.Internal.RfBlank
            else NoSignal :> IMarkerSignal
        { routing with Internal = { routing.Internal with AlcHold = value; RfBlank = rfBlank } }

    /// Set the routing of the RF blanking function. This overwrites the ALC hold function.
    let withRfBlank value routing =
        let alcHold =
            if value = (NoSignal :> IMarkerSignal) then routing.Internal.AlcHold
            else NoSignal :> IMarkerSignal
        { routing with Internal = { routing.Internal with RfBlank = value; AlcHold = alcHold } }

    /// Set the polarity of marker 1.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker1Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM1 = value; } }

    /// Set the polarity of marker 2.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker2Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM2 = value; } }

    /// Set the polarity of marker 3.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker3Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM3 = value; } }

    /// Set the polarity of marker 4.  Positive means the marker signal is high while the marker
    /// is set to true.
    let withMarker4Polarity value routing =
        { routing with MarkerPolarities = { routing.MarkerPolarities with PolarityM4 = value; } }

    /// Key to control the output routing of the BBTRIG 1 BNC.
    /// Command reference p.164.
    let private basebandTrigger1Key = ":ROUTE:CONNECTORS:BBTRIGGER1"
    /// Key to control the output routing of the BBTRIG 2 BNC.
    /// Command reference p.164.
    let private basebandTrigger2Key = ":ROUTE:CONNECTORS:BBTRIGGER2"

    /// Key to control the output routing of the EVENT 1 BNC.
    /// Command reference p.158.
    let private event1Key = ":ROUTE:CONNECTORS:EVENT"

    /// Key to control the output routing of the PAT TRIG BNC.
    /// Command reference p.164.
    let private patternTriggerKey = ":ROUTE:CONNECTORS:PTRIG"

    /// Key to control the output routing of the SWEEP OUT BNC.
    /// Command reference p.165.
    let private sweepOutKey = ":ROUTE:CONNECTORS:SOUT"

    /// Key to control the output routing of the TRIG 1 BNC.
    /// Command reference p.165.
    let private trigger1Key = ":ROUTE:CONNECTORS:TRIGGER1:OUTPUT"
    /// Key to control the output routing of the TRIG 2 BNC.
    /// Command reference p.165.
    let private trigger2Key = ":ROUTE:CONNECTORS:TRIGGER2:OUTPUT"

    /// Key to control the input routing of pattern trigger 1.
    /// Command reference p.163.
    let private patternTrigger1Key = ":ROUTE:LINE:PTRIGGER1:BNC:SOURCE"

    /// Key to control the input routing of pattern trigger 2.
    /// Command reference p.163.
    let private patternTrigger2Key = ":ROUTE:LINE:PTRIGGER2:BNC:SOURCE"

    /// Key to control the routing of the alternate amplitude function.
    /// Command reference p.329.
    let private alternateAmplitudeKey = ":RAD:ARB:MDES:AAMP"

    /// Key to control the routing of the ALC hold function.
    /// Command reference p.329.
    let private alcHoldKey = ":RAD:ARB:MDES:ALCH"

    /// Key to control the routing of the RF blanking function.  This automatically sets the ALC
    /// hold too, so sending both separately just uses the RF blanking one.
    let private rfBlankKey = ":RAD:ARB:MDES:PULS"

    /// Key to set the polarity of a given marker.
    let private markerPolarityKey = ":RAD:ARB:MPOL"

    /// Set a single output routing on the machine.
    let private setSignalRoute key route instrument =
        IO.set<ISignal> key route instrument

    /// Query a key for an IUserOutputSignal in internal representation.
    let private queryUserSignal = fun x -> IO.query Parse.userSignal x

    /// Query a key for an ISweepOutSignal in internal representation.
    let private querySweepOutSignal = fun x -> IO.query Parse.sweepOutSignal x

    /// Query a key for an ITriggerSignal in internal representation.
    let private queryTriggerSignal = fun x -> IO.query Parse.triggerSignal x

    /// Query a key for an IMarkerSignal in internal representation.
    let private queryMarkerSignal = fun x -> IO.query Parse.markerSignal x

    /// Query a key for an IUserBncSignal in internal representation.
    let private queryUserBncSignal = fun x -> IO.query Parse.userBncSignal x

    /// Make the specific marker polarity key for a given marker.
    let private makeMarkerPolarityKey marker =
        sprintf "%s:%s" markerPolarityKey (
            match marker with
            | RouteMarker1 -> "MARK1"
            | RouteMarker2 -> "MARK2"
            | RouteMarker3 -> "MARK3"
            | RouteMarker4 -> "MARK4" )

    /// Set the polarity of a single marker.
    let internal setMarkerPolarity marker value instrument =
        IO.set<Polarity> (makeMarkerPolarityKey marker) value instrument

    /// Query the polarity of a single marker.
    let internal queryMarkerPolarity marker instrument =
        IO.query Parse.polarity (makeMarkerPolarityKey marker) instrument

    /// Set the routing of the RF blank pulse channel.  This single function is only for use
    /// internally in Endorphin, since we need to set a marker to be the RF blanking pulse.
    let internal setRfBlankRoute route instrument = setSignalRoute rfBlankKey route instrument

    /// Set all the available output routes to the given values.
    let private setOutputRouting routing instrument = async {
        do! setSignalRoute basebandTrigger1Key routing.BbTrig1 instrument
        do! setSignalRoute basebandTrigger2Key routing.BbTrig2 instrument
        do! setSignalRoute event1Key routing.Event1 instrument
        do! setSignalRoute patternTriggerKey routing.PatTrig instrument
        do! setSignalRoute sweepOutKey routing.SweepOut instrument
        do! setSignalRoute trigger1Key routing.Trig1 instrument
        do! setSignalRoute trigger2Key routing.Trig2 instrument }

    /// Set all the available input routes to the given values.
    let private setInputRouting routing instrument = async {
        do! setSignalRoute patternTrigger1Key routing.PatTrig1 instrument
        do! setSignalRoute patternTrigger2Key routing.PatTrig2 instrument }

    /// Set all the available internal routes to the given values.
    let private setInternalRouting routing instrument = async {
        do! setSignalRoute alternateAmplitudeKey routing.AltAmplitude instrument
        do! setSignalRoute alcHoldKey routing.AlcHold instrument
        do! setSignalRoute rfBlankKey routing.RfBlank instrument }

    /// Set the polarities of the markers.
    let private setMarkerPolarities routing instrument = async {
        do! setMarkerPolarity RouteMarker1 routing.PolarityM1 instrument
        do! setMarkerPolarity RouteMarker2 routing.PolarityM2 instrument
        do! setMarkerPolarity RouteMarker3 routing.PolarityM3 instrument
        do! setMarkerPolarity RouteMarker4 routing.PolarityM4 instrument }

    /// Set all the routings for the machine to the given values.
    let set routing instrument = async {
        do! setOutputRouting routing.Output instrument
        do! setInputRouting routing.Input instrument
        do! setInternalRouting routing.Internal instrument
        do! setMarkerPolarities routing.MarkerPolarities instrument }

    /// Query the machine for the currently setup output routing.
    let private queryOutputRouting instrument = async {
        let! bbTrig1  = queryUserSignal basebandTrigger1Key instrument
        let! bbTrig2  = queryUserSignal basebandTrigger2Key instrument
        let! event1   = queryUserSignal event1Key instrument
        let! patTrig  = queryUserSignal patternTriggerKey instrument
        let! sweepOut = querySweepOutSignal sweepOutKey instrument
        let! trig1    = queryTriggerSignal trigger1Key instrument
        let! trig2    = queryTriggerSignal trigger2Key instrument
        return {
            BbTrig1  = bbTrig1
            BbTrig2  = bbTrig2
            Event1   = event1
            PatTrig  = patTrig
            SweepOut = sweepOut
            Trig1    = trig1
            Trig2    = trig2 } }

    /// Query the machine for the currently setup input routing.
    let private queryInputRouting instrument = async {
        let! patTrig1 = queryUserBncSignal patternTrigger1Key instrument
        let! patTrig2 = queryUserBncSignal patternTrigger2Key instrument
        return {
            PatTrig1 = patTrig1
            PatTrig2 = patTrig2 } }

    /// Query the machine for the currently setup internal routing.
    let private queryInternalRouting instrument = async {
        let! altAmp  = queryMarkerSignal alternateAmplitudeKey instrument
        let! alcHold = queryMarkerSignal alcHoldKey instrument
        let! rfBlank = queryMarkerSignal rfBlankKey instrument
        return {
            AltAmplitude = altAmp
            AlcHold      = alcHold
            RfBlank      = rfBlank } }

    /// Query the polarities of the marker signals.
    let private queryMarkerPolarities instrument = async {
        let! m1 = queryMarkerPolarity RouteMarker1 instrument
        let! m2 = queryMarkerPolarity RouteMarker2 instrument
        let! m3 = queryMarkerPolarity RouteMarker3 instrument
        let! m4 = queryMarkerPolarity RouteMarker4 instrument
        return {
            PolarityM1 = m1
            PolarityM2 = m2
            PolarityM3 = m3
            PolarityM4 = m4 } }

    /// Query the machine for the currently setup routings.
    let query instrument = async {
        let! output = queryOutputRouting instrument
        let! input = queryInputRouting instrument
        let! internal' = queryInternalRouting instrument
        let! marker = queryMarkerPolarities instrument
        return {
            Output = output
            Input = input
            Internal = internal'
            MarkerPolarities = marker } }
