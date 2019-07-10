/****************************************************************************************[Dimacs.h]
Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
Copyright (c) 2007-2010, Niklas Sorensson

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

#ifndef Minisat_Dimacs_h
#define Minisat_Dimacs_h

#include <stdio.h>

#include "minisat/utils/ParseUtils.h"
#include "minisat/core/SolverTypes.h"

namespace Minisat {

//=================================================================================================
// DIMACS Parser:

template<class B, class Solver>
static void readClause(B& in, Solver& S, vec<Lit>& lits, double topW,  vec<Lit>& asumps) {
    int     parsed_lit, vari;
    double  weight;
    lits.clear();
    weight = parseDouble(in);
    for (;;){
        parsed_lit = parseInt(in);
        if (parsed_lit == 0) break;
        vari = abs(parsed_lit)-1;
        while (vari >= S.nVars()) S.newVar();
        if (weight != topW) {
			asumps.push ( (parsed_lit > 0) ? mkLit(vari) : ~mkLit(vari) );
		} else {
			lits.push( (parsed_lit > 0) ? mkLit(vari) : ~mkLit(vari) );
		} 
        
    }
}

template<class B, class Solver>
static void parse_DIMACS_main(B& in, Solver& S,  vec<Lit>& assumptions, bool strictp = false) {
    vec<Lit> lits;
    int vars    = 0;
    int clauses = 0;
    int cnt     = 0;
    double maxW = 0;
    for (;;){
        skipWhitespace(in);
        if (*in == EOF) break;
        else if (*in == 'p'){
            if (eagerMatch(in, "p wcnf")){
                vars    = parseInt(in);
                clauses = parseInt(in);
				maxW = parseDouble(in);
            }else{
                printf("PARSE ERROR! Unexpected char: %c\n", *in), exit(3);
            }
        } else if (*in == 'c' || *in == 'p')
            skipLine(in);
        else{
            cnt++;
            readClause(in, S, lits, maxW, assumptions);
            if (lits.size() != 0)
				S.addClause_(lits); }
    }
    if (strictp && cnt != clauses)
        printf("PARSE ERROR! DIMACS header mismatch: wrong number of clauses\n");
}

// Inserts problem into solver.
//
template<class Solver>
static void parse_DIMACS(gzFile input_stream, Solver& S, vec<Lit>& assumptions, bool strictp = false) {
    StreamBuffer in(input_stream);
    parse_DIMACS_main(in, S, assumptions, strictp); }

//=================================================================================================
}

#endif
