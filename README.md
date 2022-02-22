# DDS

*DDS* project is an implementation of theroetical framework of *Distributive Deductive Systems*, written in its entirety in Haskell.

The main idea behind *DDS* is to combine in one consistent frameork different well-known proof systems, such as sequent calculi, analytic tableaux etc. as well as less famous method of synthetic tableaux. Different proof methods are used to analyze formulas depending on their syntactic features (as described in module *Decision Tree*). We have focused on the treatment of Classical Propositional Logic, but formalization of other logics has already began, especially the implementation of basic logic of formal inconsistency, *mbC* system.

## Project structure
Location */src* contains four subdirectories. Three of them, CPL, MBC, SCI contains source files which implement three logics, classical propositional logic, mbC system and SCI (Sentential Calculus with Identity - basic non-Fregean logic). Directory */src/GEN* contains source files devoted to generation of large datasets used in the analysis of different proof-search heuristics in Synthetic Tableaux method. We will describe this metod in the next section. Only module *CPL* is finished, the other modules are still under construction and at this point there is no point of using them. Location */src/CPL* caontins implementations of different proof methods for CPL, *can_specific* refers to sequent calculus, *dual_specific* to resolution method, *ke_specific* to the version of analytic tableaux with the rule of cut, and *general* contains modules describing some basic structures used in each considered proof system as well as original implementation of the Random instance for the type of propositional formulas (*RandomFor2*).

## Synthetic tableaux
The implementation of synthetic tabelaux (*ST*) is located in */src/CPL/synth_specific*, where the most important module is that of *FlexibleTrees*. It enables the user to build a synthetic tableaux proof given a certain instruction which specify the order of atoms introduced by the cut rule. It is also possible to consider all possible instructions (both regular and not). Another *ST* related modules are located in *src/GEN*. Module *GenSynth5* for example generates a set of random formulas of some fixed length having exactly 5 different propositional variables. For each formula all possible synthetic tress are build and some basic feature of those are generated in a .txt file. Module *Pigeon* enables the user to check the complexity of synthetic trees (generated according to a random regular instruction) for various formulas falling under the scheme of pigeonhole principle. All files form this drectory are able to produce *IO()* result and thus can be called from the */app/Main* by means of a stack command: *stack exec DDS-exe*. 

## Data concerning experiments with ST for IJCAR 2022

File "/data/results_per_formula.txt" contains the following data:

1st column contains a formula, each row describes one formula
for_length 	= length of the formula, that is, the number of characters, parentheses excluded
no_diff_vars 	= number of different variables in the formula
rangeb 		= the difference between the size of a maximal tableau and that of a minimal tableau, where the size is calculated as the number of branches; called "range" in the paper
POT 		= the value of POT function; see the paper
if_opt 		= whether the indicated tableau is of the optimal size (size = no of branches)
diff_indic_opt 	= the difference between the size of the indicated tableau and the optimal size
dpmax 		= maximal value of dp function on literals

File "/data/dp_values.txt" contains the following data:

1st column contains a formula, one row corresponds to one leaf of the syntactic tree of the formula, hence each formula is described in a number of rows
for_length = length of the formula, that is, the number of characters, parentheses excluded
no_diff_vars = number of different variables in the formula
dp1 = the value dp(formula,literal) for "formula" in the first column, "literal" in the last column; see the paper
dp2 = the value dp(~formula,literal*) for the negation of the formula in the first column, "literal*" is the dual of literal in the last column
