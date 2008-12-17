/*
 * VClock.h
 *
 * This header contains the declaration of the VClock puzzle class,
 * The VClock puzzle is just like clock, except that the amount
 * of movement is variable and there can be multiple amounts.
 *
 * Author: Noah Richards
 *
 * $Id: VClock.h,v 1.3 2006/04/10 15:50:42 noah Exp $
 */


#ifndef VCLOCK_H
#define VCLOCK_H

#include <vector>
#include <iostream>
#include <sstream>
#include "Puzzle.h"
using std::vector;
using std::string;
using std::stringstream;

class VClock : public Puzzle<int>
{
public:
	typedef int config_type;

	class InvalidVClockParameters : public std::exception
	{
		const char* what() const throw()
		{
			return "Invalid parameters supplied to vclock constructor.\n"
				"All values must be positive integers, and start and "
				"end must be less than or equal to numHours. Also, "
				"the movements vector must be non-empty.";
		}
	};

	explicit VClock(int numHours, int start, int end,
			const vector<int>& movements) :
		_numHours(numHours), _start(start), _end(end),
		_movements(movements) {};

	vector<config_type> getNeighbors(const config_type&) const;
	bool isSolution(const config_type&) const;
	config_type getStartConfig() const;
	string stringFromConfig(const config_type&) const;

private:
	config_type _numHours;
	config_type _start;
	config_type _end;
	vector<int> _movements;
};

vector<VClock::config_type> VClock::getNeighbors(const config_type& cfg) const
{
	vector<config_type> neighbors;

	for(vector<int>::const_iterator iter = _movements.begin();
			iter != _movements.end();
			iter++)
	{
		config_type nextval = cfg;
		nextval += *iter;

		while(nextval < 1)
			nextval += _numHours;

		while(nextval > _numHours)
			nextval -= _numHours;

		neighbors.push_back(nextval);
	}

	return neighbors;
}	

bool VClock::isSolution(const config_type& cfg) const
{
	return cfg == _end;
}

VClock::config_type VClock::getStartConfig() const
{
	return _start;
}

string VClock::stringFromConfig(const config_type& cfg) const
{
	stringstream s;
	s << cfg;
	return s.str();
}

#endif

/* Change log:
 *
 * $Log: VClock.h,v $
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
 * Revision 1.1  2006/01/28 05:12:49  noah
 * Initial revision
 */
