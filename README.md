# path-specific-causal-decomp
Simulation code for "Path-specific causal decomposition analysis with multiple correlated mediator variables" by Melissa J. Smith, Leslie A. McClure, and D. Leann Long

To reproduce the simulation study results, you can run the code files in the following order:

- Run computeTrueEffects.R to compute and save the true values of the decomposition effects.
- Run each of the shell scripts called decompSim1.sh, decompSim2.sh, ... , decompSim18.sh on a high performance computer. These batch jobs run simulation iterations in parallel. Each will produce 1000 separate files where the results from that run of the scenario are saved.
- Run compileResultsSimulation.R after obtaining the simulation results to summarize the results and generate plots.

The following R scripts are used in the simulation runs:

- generateDecompData.R: generates the data and can be used to compute true effects.
- runExistingMethod.R: runs the existing method proposed by Sudharsanan and Bijlsma and implemented by Sudharsanan and Ho for two mediators (two continuous, two binary, or one continuous and one binary).
- runProposedMethod.R: runs the proposed method in this paper for two mediators (two continuous, two binary, or one continuous and one binary).
- runOneInterationSimulation: combines all the steps of the simulation study - generating the data, applying the proposed and existing methods, bootstrapping, returning the estimates and 95% confidence intervals for the decomposition effects, and saving the output to a file.


