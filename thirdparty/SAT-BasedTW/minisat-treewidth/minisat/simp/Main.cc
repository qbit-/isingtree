/*****************************************************************************************[Main.cc]
Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
Copyright (c) 2007,      Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

#include <errno.h>
#include <zlib.h>

#include "minisat/utils/System.h"
#include "minisat/utils/ParseUtils.h"
#include "minisat/utils/Options.h"
#include "minisat/core/Dimacs.h"
#include "minisat/simp/SimpSolver.h"

using namespace Minisat;

//=================================================================================================


static Solver* solver;
// Terminate by notifying the solver and back out gracefully. This is mainly to have a test-case
// for this feature of the Solver as it may take longer than an immediate call to '_exit()'.
static void SIGINT_interrupt(int) { solver->interrupt(); }

// Note that '_exit()' rather than 'exit()' has to be used. The reason is that 'exit()' calls
// destructors and may cause deadlocks if a malloc/free function happens to be running (these
// functions are guarded by locks for multithreaded use).
static void SIGINT_exit(int) {
    printf("\n"); printf("*** INTERRUPTED ***\n");
    if (solver->verbosity > 0){
        solver->printStats();
        printf("\n"); printf("*** INTERRUPTED ***\n"); }
    _exit(1); }


//=================================================================================================
// Main:

int main(int argc, char** argv)
{
    try {
        setUsageHelp("USAGE: %s [options] <input-file> <result-output-file>\n\n  where input may be either in plain or gzipped DIMACS.\n");
        setX86FPUPrecision();
        
        // Extra options:
        //
        IntOption    verb   ("MAIN", "verb",   "Verbosity level (0=silent, 1=some, 2=more).", 1, IntRange(0, 2));
        BoolOption   pre    ("MAIN", "pre",    "Completely turn on/off any preprocessing.", true);
        BoolOption   solve  ("MAIN", "solve",  "Completely turn on/off solving after preprocessing.", true);
        StringOption dimacs ("MAIN", "dimacs", "If given, stop after preprocessing and write the result to this file.");
        IntOption    cpu_lim("MAIN", "cpu-lim","Limit on CPU time allowed in seconds.\n", 0, IntRange(0, INT32_MAX));
        IntOption    mem_lim("MAIN", "mem-lim","Limit on memory usage in megabytes.\n", 0, IntRange(0, INT32_MAX));
        BoolOption   strictp("MAIN", "strict", "Validate DIMACS header during parsing.", false);
        
        IntOption   search("MAIN", "search", "Search strategy (1=Iter-Bin, 2=Iter-Lin-U, 3=Iter-Lin-D.", 1, IntRange(1,3));

        parseOptions(argc, argv, true);
        
        SimpSolver  S;
        double      initial_time = cpuTime();

        S.verbosity = verb;
        
        solver = &S;
        // Use signal handlers that forcibly quit until the solver will be able to respond to
        // interrupts:
        sigTerm(SIGINT_exit);

        // Try to set resource limits:
        if (cpu_lim != 0) limitTime(cpu_lim);
        if (mem_lim != 0) limitMemory(mem_lim);

        if (argc == 1)
            printf("Reading from standard input... Use '--help' for help.\n");

        gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");
        if (in == NULL)
            printf("ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]), exit(1);
        
        if (S.verbosity > 0){
            printf("============================[ Problem Statistics ]=============================\n");
            printf("|                                                                             |\n"); }
        
        vec<Lit> assumptions;
        assumptions.clear();
        parse_DIMACS(in, S, assumptions, (bool)strictp);
        gzclose(in);
        FILE* res = (argc >= 3) ? fopen(argv[2], "wb") : NULL;
            
        if (S.verbosity > 0){
            printf("|  Number of variables:  %12d                                         |\n", S.nVars());
            printf("|  Number of clauses:    %12d                                         |\n", S.nClauses()); }
        
        double parsed_time = cpuTime();
        if (S.verbosity > 0)
            printf("|  Parse time:           %12.2f s                                       |\n", parsed_time - initial_time);

        // Change to signal-handlers that will only notify the solver and allow it to terminate
        // voluntarily:
        sigTerm(SIGINT_interrupt);

       if (!S.okay()){
            if (res != NULL) fprintf(res, "UNSAT\n"), fclose(res);
            if (S.verbosity > 0){
                printf("===============================================================================\n");
                printf("Solved by simplification\n");
                S.printStats();
                printf("\n"); }
            printf("UNSATISFIABLE\n");
            exit(20);
        } 
		
		
		lbool ret = l_Undef;
		vec<Lit> currentAssumptions;
		double iteration = 0;
		int treeW = -1;
		if (search==1) {
			 // Binary search loop
			int lb = 0;
			int ub = assumptions.size();
			while(lb < ub) {
				treeW = lb + ((ub - lb) / 2);
				if (treeW == lb) {
					treeW++;
				}
				printf("LB: %d, UB: %d, Treew: %d\n", lb, ub, treeW);
				
				currentAssumptions.clear();
				assumptions.copyTo(currentAssumptions);
				
				for(int i = 0; i < treeW; i++) {
					currentAssumptions[i] = ~currentAssumptions[i];
				} 
				

				//printf("Assumptions to Solver\n");
					//for(int i = 0; i < currentAssumptions.size(); i++) {
						//printf(" %c%d", sign(currentAssumptions[i]) ? '-' : ' ', var(currentAssumptions[i]));
					//} 
				//printf("\n");

							
				if (solve){
					ret = S.solveLimited(currentAssumptions);
					iteration = cpuTime();
				}else if (S.verbosity > 0)
					printf("===============================================================================\n");

				if (dimacs && ret == l_Undef)
					S.toDimacs((const char*)dimacs);

				if (S.verbosity > 0){
					S.printStats();
					printf("\n"); }
				printf(ret == l_True ? "SATISFIABLE\n" : ret == l_False ? "UNSATISFIABLE\n" : "INDETERMINATE\n");
				
				if (res != NULL){
					if (ret == l_True){
						printf("Printing sol\n");
						fprintf(res, "s SAT\no");
						for (int i = 0; i < S.nVars(); i++)
							if (S.model[i] != l_Undef)
								fprintf(res, "%s%s%d", (i==0)?"":" ", (S.model[i]==l_True)?"":"-", i+1);
						fprintf(res, " 0\n");
						fclose(res);
					}
				}
				if (ret ==  l_True) {
					if (treeW == (lb + 1)) {
						printf("Proven treewidth of given graph %d\n", treeW); 
						printf("Total time  %12.2f s                                       \n", iteration - initial_time);
						break;
					} else {
						printf("Current LB %d, UB, %d\n", lb, treeW);
						printf("Time so far  %12.2f s                                       \n", iteration - initial_time);
						ub = treeW;
					}
					
				} else if (ret ==  l_False) {
					printf("Current LB %d, UB, %d\n", treeW, ub);
					printf("Time so far  %12.2f s                                       \n", iteration - initial_time);
					lb = treeW;
				} else {
					printf("Something went wrong..\n");
					break;
				}
				
			}
		}
		
		if (search==2) {
		// LINEAR UPWARDS SEARCH LOOP
			treeW = 1;
			
			while(treeW < assumptions.size()) {
				currentAssumptions.clear();
				assumptions.copyTo(currentAssumptions);
				
				for(int i = 0; i < treeW; i++) {
					currentAssumptions[i] = ~currentAssumptions[i];
				} 
					
				if (solve){
					ret = S.solveLimited(currentAssumptions);
					iteration = cpuTime();
				}else if (S.verbosity > 0)
					printf("===============================================================================\n");

				if (dimacs && ret == l_Undef)
					S.toDimacs((const char*)dimacs);

				if (S.verbosity > 0){
					S.printStats();
					printf("\n"); }
				printf(ret == l_True ? "SATISFIABLE\n" : ret == l_False ? "UNSATISFIABLE\n" : "INDETERMINATE\n");
				
					
				
				if (res != NULL){
					if (ret == l_True){
						printf("Printing sol\n");
						fprintf(res, "s SAT\no");
						for (int i = 0; i < S.nVars(); i++)
							if (S.model[i] != l_Undef)
								fprintf(res, "%s%s%d", (i==0)?"":" ", (S.model[i]==l_True)?"":"-", i+1);
						fprintf(res, " 0\n");
						fclose(res);
					}
				}
				if (ret ==  l_True) {
					printf("Treewidth of given graph %d\n", treeW); 
					printf("Total time  %12.2f s                                       \n", iteration - initial_time);
					break;
				} else {
					printf("Current lower bound (additionally to what was calculated when creating the instance), %d\n", treeW);
					printf("Time so far  %12.2f s                                       \n", iteration - initial_time);
					treeW++;
				}
				
			}
		}
	 
	 
	 // LINEAR DOWNWARDS SEARCH LOOP
	 if (search==3) {
		  treeW =  assumptions.size();
			while(treeW > 0) {
				currentAssumptions.clear();
				assumptions.copyTo(currentAssumptions);
				
				for(int i = 0; i < treeW; i++) {
					currentAssumptions[i] = ~currentAssumptions[i];
				} 
					
				if (solve){
					ret = S.solveLimited(currentAssumptions);
					iteration = cpuTime();
				}else if (S.verbosity > 0)
					printf("===============================================================================\n");

				if (dimacs && ret == l_Undef)
					S.toDimacs((const char*)dimacs);

				if (S.verbosity > 0){
					S.printStats();
					printf("\n"); }
				printf(ret == l_True ? "SATISFIABLE\n" : ret == l_False ? "UNSATISFIABLE\n" : "INDETERMINATE\n");
				

				if (ret ==  l_True) {
					printf("Upper bound of given graph %d\n", treeW); 
					printf("Time so far  %12.2f s                                       \n", iteration - initial_time);
					treeW--;
				} else {
					printf("Proven TW of graph, %d\n", treeW + 1);
					printf("Total Time  %12.2f s                                       \n", iteration - initial_time);
					
					break;
				}
				
		 }
	}
	 

#ifdef NDEBUG
        exit(ret == l_True ? 10 : ret == l_False ? 20 : 0);     // (faster than "return", which will invoke the destructor for 'Solver')
#else
        return (ret == l_True ? 10 : ret == l_False ? 20 : 0);
#endif
    } catch (OutOfMemoryException&){
        printf("===============================================================================\n");
        printf("INDETERMINATE\n");
        exit(0);
    }
}
