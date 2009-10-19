/*
 * Solver.h
 *
 * This header contains the declaration of the Solver class, 
 * which, given a Puzzle, "solves" the puzzle.
 *
 * Author: Noah Richards
 *
 * $Id: Solver.h,v 1.4 2006/04/20 21:46:56 noah Exp $
 */


#ifndef SOLVER_H
#define SOLVER_H

#include <iostream>
#include <queue>
#include <list>
#include <map>
#include <cassert>
#include "Puzzle.h"
using namespace std;

namespace solver {

template <typename T>
void solve(Puzzle<T>& puzzle, ostream& out = cout)
{
	unsigned int nConfigs = 0;

	typedef typename Puzzle<T>::config_type config_type;

	// Setup queue of configurations - this is where we
	// will construct the "tree" of actions
	config_type start = puzzle.getStartConfig();
	assert(start == start);
	if(puzzle.isSolution(start))
	{
		out << "Start configuration is solution; no steps necessary"
		     << endl;
		return;
	}

	queue<config_type> cfgQueue;

	// Setup a map to record all visited configuration
	// and for memoization (backtracking path)
	typedef map<config_type, config_type> cfg_map;
	cfg_map visited;
	visited[start] = start;

	// Initialize the configuration queue with the first
	// value
	cfgQueue.push(start);

	bool solutionFound = false;
	config_type solution;

	// While the queue is not empty, continue processing
	while(cfgQueue.size())
	{
		config_type currentCfg = cfgQueue.front();
		cfgQueue.pop();

		// Get the neighbors of the current config, which is a vector
		// of configurations
		vector<config_type> neighbors = puzzle.getNeighbors(currentCfg);

		// For each neighbor, check and see if it has been visited.  If
		// it hasn't, add it to the visited and check if it is a solution.
		// If it is, we are done.
		for(typename vector<config_type>::iterator iter = neighbors.begin();
				iter != neighbors.end();
				iter++)
		{
			if(visited.insert(make_pair(*iter, currentCfg)).second)
			{
				nConfigs++;

				if(puzzle.isSolution(*iter))
				{
					solutionFound = true;
					solution = *iter;
					break;
				}

				cfgQueue.push(*iter);
			}
		}

		// In leiu of a goto - the initial break will only break out
		// of the immediate for loop
		if(solutionFound)
			break;
	}

	// If a solution has been found (meaning that the previous loop
	// didn't terminate due to an empty queue), display the
	// steps required in the solution
	if(solutionFound)
	{
		// Walk through the path using the map.  Use
		// push_front so that the resulting list is
		// in the proper order (start -> finish)
		list<config_type*> path;

		config_type* config = &solution;

		while(*config != start)
		{
			path.push_front(config);
			config = &visited[*config];
		}
		path.push_front(&start);

		out << "Solution path:" << endl;
		
		int count = 0;

		// Iterate over the resulting list and display each configuration,
		// using the puzzle's stringFromConfig method
		for(typename list<config_type*>::iterator iter = path.begin();
				iter != path.end();
				iter++)
		{
			out << "Move #" << count++ << ":\n" 
			     << puzzle.stringFromConfig(**iter) << endl;
		}
	}
	else
		out << "No solution." << endl;

	out << "Number of unique configurations created/tested: " 
	     << nConfigs << endl;
}

} // namespace solver

#endif

/* Change log:
 *
 * $Log: Solver.h,v $
 * Revision 1.4  2006/04/20 21:46:56  noah
 *
 * Minor cleanups.
 *
 * Revision 1.3  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.2  2006/03/27 04:59:04  noah
 * Replaced Solver class with single templated function.
 * Created a templated Puzzle base class for the templated function
 * to use, which formalizes requirements on the puzzle types and
 * allows the templated solve function to guess its arguments.
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 * CS4 Project
 *
 * Revision 1.6  2006/02/08 06:06:42  noah
 * Solver now records number of unique configurations to tell
 * user at end of run.
 *
 * Revision 1.5  2006/01/29 03:19:10  noah
 * Correct command line argument usage (filenam or "-")
 *
 * Revision 1.4  2006/01/28 05:12:49  noah
 * Change Puzzle
 *
 * Revision 1.3  2006/01/26 18:01:00  noah
 * Added status information
 *
 * Revision 1.2  2006/01/26 04:49:35  noah
 * Removed inheritence from Puzzle
 *
 * Revision 1.1  2006/01/25 07:02:04  noah
 * Initial revision
 */
