# Repositories & Branches

This `eba-development` repository is the bleeding edge development repository where the most recent features, bug checkers and experiements can be found. These will -- when ready -- be merged into the main [eba](https://github.com/IagoAbal/eba) repository. 

The features not yet ready for integration into the main `eba` repository are at the time of writing:
- `use-after-free-monitor`: A monitor template-based bug checker for UAF bugs in the Linux kernel
- `print-cfg`: A commandline parameter for printing a detailed view of the internal CFG found in `eba` when verifying files 
- `automata-helper-refactor`: A future refactor of the shared logic between monitor templates 

A collection of bugs used for verifying the performance of developed bug checkers can be found in [this](https://github.com/itu-square/eba-kernel-bugs) repository.