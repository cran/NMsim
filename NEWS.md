# NMsim 0.1.2
No changes since 0.1.1 except for disabling a test that was failing on
some systems due to technical reasons.

# NMsim 0.1.1
While no critical and generally only few bugs have been found in NMsim
0.1.0, NMsim 0.2.0 includes several improvements and upgrades. The
interface feels smoother too. I want to thank Ron Keizer for feedback
and advice.

## New features
* `NMsim()` has a much reduced and improved messaging to the user. If
  more than one model or data set is supplied or generated, progress
  trackers will be shown while starting Nonmem, while waiting for
  Nonmem to finish, and while collecting the simulation results. 
  
* The messages include information about where intermediate files and
  final results files are stored.
  
* `NMexec()` has improved support for
  estimation. `method.execute="nmsim"` and `method.execute="psn"` both
  work on linux and windows, even though less thoroughly tested on
  windows. Thank you to Boris Grinshpun for testing.

* Names of files containing final results from `NMsim()` have been
  renamed to be more intuitive. The previous `_paths.rds` will now be
  called `_MetaData.rds`. The results, once read and compressed, will
  be in a file called `_ResultsData.fst`. Notice, both these files are
  required to fully recover simulation results.  Thanks to Brian
  Reilly for discussions on this and many other design aspects.

* It is now possible to provide specific parameters (`THETA`, `OMEGA`
  and `SIGMA`) for Nonmem simulation. `NMsim()` table for
  simulations. See argument `file.ext` and `NMsim_VarCov`'s argument
  `ext`.

* New arguments to control seeds. `NMsim` can either use R's
  `set.seed` before generating the seeds for Nonmem. Detailed control
  of the seeds, including how many to include and the distribution of
  the random sources in Nonmem, can be controlled using the `seed.nm`
  argument. This way, the user can add random processes to the
  estimated control stream. The actual Nonmem seed values can also be
  provided.

* `method.sim=NMsim_typical()` has been replaced by argument
  `typical=TRUE`. This means typical subject simulations can now be
  combined with other simulations methods like `NMsim_VarCov`.

* `NMsim()` now adds a column called `sim` which carries the name of
  the simulation defined by the `name.sim` argument.

* Several checks for existence and consistency of files are
  implemented.

* The native Nonmem execution method now also works for estimation.

* `pnm` files are now saved with the model for transparency. 

## Bugfixes 
* Running `rbind` on results from `NMsim` would throw errors. Thanks
  to Simone Cassani for reporting this. Fixed.

* Using other file name extensions than `.mod` on input control
  streams in combination with `NMdataConf(file.mod)` would make NMsim
  fail. Thanks to Brian Reilly for reporting. Fixed.
  
## Other changes

* `NMsim_known()` renamed to `NMsim_EBE()`.

* Generated control streams have been stripped off of the "NMsim_"
  prefix. These files are located in `NMsim` generated folders so the
  prefix was uninformative.

* In case of multi-threaded (cluster) execution and something went
  wrong `NMexec()` used to write some output files from Nonmem in the
  current working directory. All these are now being written to the
  model execution directory for clarity and tidyness.

# NMsim 0.1.0
For the first time NMsim works on Windows. There may still be some
limitations but initial testing looks very promising. Make sure to set
`path.nonmem`. See the configuration vignette on the website:
[`NMsim-config.html`](https://philipdelff.github.io/NMsim/articles/NMsim-config.html)

0.1.0 is also an important upgrade that solidifies the way NMsim reads
results from simulations.  In addition to important bug fixes, it
allows for NMsim to wait on Nonmem to complete simulations - even when
they are run on a cluster. This means even large simulations with
NMsim can be integrated in scripts.

## New features
* Works on Windows - at least most features do.

* `NMsim()` and `NMreadSim()` now have `wait` arguments which controls
  if they will wait for Nonmem to finish simulating. This will also
  work if jobs were sent to the cluster.
  
* `NMsim()` respects the `reuse.results` argument. If `TRUE` it will
  use results file on the file system. This can be used in stead of
  putting `NMsim()` calls inside an if-statement to disable the
  simulation but read results on file.
  
* `NMsim()` looks for a couple of features of the provided control
  streams that are known to be able to cause issues. Warnings will be
  issued if these are found.
  
* `addEVID2` has a new argument, `EVID` to specify what value the
  `EVID` column should have. It can be useful sometimes to use
  `EVID=0` for simulation records.

## Bugfixes
* In some cases `NMreadSim()` would not get the path right to the
  simulation results leading to failures in reading simulation
  results. Fixed.

## Other changes
* Functions `NMreadExt` and `NMreadPhi` have been removed from
  NMsim. They live and are being maintained in the `NMdata`
  package. In NMsim, were deprecated and unmaintained functions.

# NMsim 0.0.10
NMsim 0.0.9 had an unfortunate bug in `NMreadSim()` which has been
fixed. That bugfix is difference between 0.0.9 and 0.0.10.

# NMsim 0.0.9
NMsim 0.0.9 is almost identical to 0.0.8 but ensures compatibility
with older R versions. 

## Bugfixes
* In some cases `NMreadSim` would not be able to read and combine
  results from models that returned different data variables. Fixed.

# NMsim 0.0.8

## New features
* `NMsim` 0.0.1 would generate an `rds` file with paths to simulation
  files and results for each model+data set simulated. This has been
  changed to now only generate one table per model. This makes it
  simpler to read simulation results in some cases.
  
* `NMreadSim` should now be the best and only way for the user to read
  `NMsim` simulation results. It interprets `rds` files (which are the
  ones intended for reading), `fst` files, tables of `NMsim` runs, and
  `NMsim` results. This makes it less confusing what can be processed
  by `NMreadSim` and also it sometimes easier to generalize code
  reading simulation results. Also, `NMsim` now always reads results
  using `NMreadSim`. This has the advantage that an fst file will
  always be produced if `NMsim` waits to read the results.
  
* `NMreadSim` has a new argument, `check.time` by default disabling
  checking whether a collected `fst` file is newer than the results
  files generated by `NMsim`. Normally, it's a good thing to check
  this but some ways of sharing file files may not retain file
  modification times needed to check for this. `NMsim` will delete the
  `fst` files if it finds any so normally it should not be a problem
  to skip this check.

* `modify.model` is the argument to use to modify the control stream
  after `NMsim` is done preparing the simulation. A couple of helper
  functions are available making it really easy to add contents (very
  commonly used) or modify contents. 

* `NMsim` now tries to reuse stored results if
  `reuse.results=TRUE`. It does so in a simple way - if they exist,
  they will be attempted read - so be careful to rerun simulations
  without this option if you change any arguments.
  
* `NMsim` will by default add a `DV` column with `NA` values if `DV`
  is not in input data. Nonmem most often needs that column, and it is
  uninformative for simulations. Disable this feature by using
  `auto.dv=FALSE`.
  
* The `transform` option has been integrated into the table of
  simulations created by `NMsim()`. This means even if the results are
  not read by `NMsim` (because the simulation is not executed or it is
  submitted to a cluster), the transformation will still be applied by
  `NMreadSim()` later.

* `NMsim()'s` `dir.sims` and `dir.res` arguments can be controlled
  using `NMdata::NMdataConf()`. Often these two arguments are used all
  the time, so it's convenient to be able to configure those once and
  for all in a script.

## Bugfixes

* `NMreadSim` was only able to read results if the current working
  directory was the same as when `NMsim` was executed. Now fixed.

* In some cases `NMsim` would fail on models with multiple output
  tables when the `table.vars` argument was not used. Fixed.

* `NMsim`'s `sim.dir.from.scratch` argument was not respected due to a
  simple bug, leading to `dir.sims` growing each time a simulation was
  rerun.
  
* In case simulation data is a list of data sets `NMsim` would not
  order columns when `order.columns` was `TRUE`. Now fixed.
  
* In case of lists of data sets, and the list element (data set) names
  included spaces, `NMsim()` would throw and error. Spaces in data set
  names are now replaced with under scores ("_") to avoid that. It
  will often happen when data sets are split into lists using
  `data.table::split.data.table()` - which is an excellent way to do
  this, by the way.

* Function `simPopEtas()` was not exported, so only available as
  `NMsim:::simPopEtas()`. Fixed.
  

# NMsim 0.0.7
## New features

* Function `simPopEtas()` to generate a population from a model. The
  population can be saved as a `phi` file to be reused in subsequent
  simulations. The function is by mistake not exported in 0.0.7 so for
  now you must use `NMsim:::simPopEtas()` to use it.

* Function `NMreadSim()` provides a very simple interface to reading
  simulation results. Especailly in cases where the simulation is
  being parallelized or otherwise spawns multiple Nonmem jobs, this is
  a useful feature.
  
* A list of simulation data sets will now be simulated with separate
  Nonmem runs. This is an efficient way to parellelize large
  simulation runs. 

# NMsim 0.0.6
## New features

* Support for parallelization of simulations added when using PSN. It
used to be possible to run multiple simulations simultaneously in
separate threads. Now single simulation runs can be parallelized on
`sge` type clusters (using `qsub`). See arguments `sge` and `nc`.

## Bugfixes

* A simple mistake would create problems in `genPhiFile()` when having
  more than 10 ETAs in a model. Now fixed.

# NMsim 0.0.5
## New features

* Full support for models estimated with SAEM. Especially, simulation
  of "known" subjects, i.e. re-using emperical Bayes estimates, is
  slightly different with these models.

* Experimental support for windows with PsN. `dir.psn` argument has to
  point to a directory where executables `execute` and `update_inits`
  are found. Thanks to Sjoerd Koopman for debugging and testing
  this. Hopefully in future versions, `PsN` will not be needed on
  Windows (like it is not needed on Linux).

* The simulation method called NMsim_known now accepts other `.phi`
  files to use than the .phi file generated by the estimation
  run. This is useful if one wants to reuse subjects generated in a
  previous simulation.

## Other/minor improvements

* NMexec now also copies out the shk (shrinkage estimates) file after
  a run. The files that will by default be copied and reported next to
  the control streams are now `xml`, `ext`, `cov`, `cor`, `coi`,
  `phi`, `shk` - in addition to output table files and the archived
  input data of course.

# NMsim 0.0.2
## New features

* NMsim supports `type.sim="typical"` which means all OMEGAS will be
  fixed to zero. This requires the ext file to be present.

* Experimental support for simulation of estimated subjects using
  `type.sim="known"`.
