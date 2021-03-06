#### 0.3.6 - 2017-01-31
* Add readable log4net logging at this level
* Correct SetSweepType serialisation

#### 0.3.5 - 2016-11-02
* Correct transposition in model number

#### 0.3.4 - 2016-11-01
* Expose types needed to access the instrument without namespace collisions

#### 0.3.3 - 2016-10-04
* Rename instrument level function module and auto-open. Use namespace to discriminate

#### 0.3.2 - 2016-09-26
* Correct query amplitude to return a value
* Expose open/close through RfSource

#### 0.3.1 - 2016-09-26
* Return type-hiding so experiments can tell what kind of SCPI instrument this is
* Add a method to access the SCPI instrument

#### 0.3.0 - 2016-08-11
* Move to Endorphin.Core.SCPI-based IO, and remove type-hiding on the instrument
  so commands may be sent manually.
* Refactor functions to be consistent - the instrument is now always the last
  argument.

#### 0.2.0 - 2016-08-02
* Change to synchronous IO operations to prevent memory errors in VISA code

#### 0.1.2 - 2016-08-01
* Fix unit-of-measure referencing issues with Endorphin.Core

#### 0.1.1 - 2016-07-01
* Update to new Endorphin.Core API

#### 0.1.0-beta1 - 2016-06-28
* Initial open-source release
