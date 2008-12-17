/*
 * Clock.h
 *
 * This header contains the declaration of the Clock puzzle class,
 * which publicly inherits from Puzzle.
 *
 * Author: Noah Richards
 *
 * $Id: Clock.h,v 1.4 2006/04/24 13:42:57 noah Exp $
 *
 */


#ifndef CLOCK_H
#define CLOCK_H

#include <vector>
#include <iostream>
#include <sstream>
#include "Puzzle.h"
/*using std::vector;
using std::string;
using std::stringstream;
*/
using namespace std;

class Clock : public Puzzle<int>
{
public:
	class InvalidClockParameters : public std::exception
	{
		const char* what() const throw()
		{
			return "Invalid parameters supplied to clock constructor.\n"
				"All values must be positive integers, and start and "
				"end must be less than or equal to numHours.";
		}
	};

	explicit Clock(int numHours, int start, int end) :
		_numHours(numHours), _start(start), _end(end) {};

	vector<int> getNeighbors(const int&) const;
	bool isSolution(const int&) const;
	int getStartConfig() const;
	string stringFromConfig(const int&) const;

private:
	int _numHours;
	int _start;
	int _end;
};

vector<int> Clock::getNeighbors(const int& cfg) const
{
	vector<int> neighbors;

	// Next neighbor (moving counter-clockwise)
	int next = cfg - 1;
	if(!next) next = _numHours;
	neighbors.push_back(next);

	// Next neighbor (moving clockwise)
	next = cfg + 1;
	if(next > _numHours) next = 1;
	neighbors.push_back(next);

	return neighbors;
}	

bool Clock::isSolution(const int& cfg) const
{
	return cfg == _end;
}

int Clock::getStartConfig() const
{
	return _start;
}

string Clock::stringFromConfig(const int& cfg) const
{
	stringstream s;
	s << cfg;
	return s.str();
}

#endif

/* Change log:
 *
 * $Log: Clock.h,v $
 * Revision 1.4  2006/04/24 13:42:57  noah
 *
 *
 * Added water start.
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
 * Revision 1.2  2006/01/26 04:49:35  noah
 * Removed inheritence from Puzzle
 *
 * Revision 1.1  2006/01/25 07:02:04  noah
 * Initial revision
 */
