/*
 * Change.h
 *
 * This header contains the declaration of the Change puzzle class,
 *
 * Author: Noah Richards
 *
 * $Id: Change.h,v 1.3 2006/04/10 15:50:42 noah Exp $
 */


#ifndef CHANGE_H
#define CHANGE_H

#include <vector>
#include <iostream>
#include <sstream>
#include "Puzzle.h"
using namespace std;

class Change : public Puzzle<int>
{
public:
	Change(int amount, const vector<int>& denominations);

	vector<int> getNeighbors(const int&) const;
	bool isSolution(const int&) const;
	int getStartConfig() const;
	string stringFromConfig(const int&) const;

private:
	
	int _end;
	vector<int> _denominations;
};

Change::Change(int amount, const vector<int>& denominations) :
	_end(amount), _denominations(denominations)
{ }



vector<int> Change::getNeighbors(const int& cfg) const
{
	vector<int> neighbors;

	for(vector<int>::const_iterator iter = _denominations.begin();
			iter != _denominations.end();
			iter++)
	{
		int newval = cfg + *iter;
		if(newval <= _end)
			neighbors.push_back(newval);
	}

	return neighbors;
}	

bool Change::isSolution(const int& cfg) const
{
	return cfg == _end;
}

int Change::getStartConfig() const
{
	return 0;
}

string Change::stringFromConfig(const int& cfg) const
{
	stringstream s;
	s << cfg;
	return s.str();
}

#endif

/* Change log:
 *
 * $Log: Change.h,v $
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
 * Revision 1.3  2006/01/29 03:19:10  noah
 * Correct command line argument usage (filenam or "-")
 *
 * Revision 1.2  2006/01/28 05:12:49  noah
 * Change Puzzle
 *
 * Revision 1.1  2006/01/26 18:01:00  noah
 * Initial revision
 *
 */
