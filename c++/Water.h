/*
 * Water.h
 *
 * This header contains the declaration of the Water puzzle class,
 *
 * Author: Noah Richards
 *
 * $Id: Water.h,v 1.1 2006/04/24 13:42:57 noah Exp $
 */


#ifndef WATER_H
#define WATER_H

#include <vector>
#include <iostream>
#include <sstream>
#include "Puzzle.h"
using namespace std;

class Water : public Puzzle<vector<int> >
{
public:
	Water(int amount, const vector<int>& buckets);

	vector< vector<int> > getNeighbors(const vector<int>&) const;
	bool isSolution(const vector<int>&) const;
	vector<int> getStartConfig() const;
	string stringFromConfig(const vector<int>&) const;

private:
	
	int _end;
	vector<int> _buckets;
};

Water::Water(int amount, const vector<int>& buckets) :
	_end(amount), _buckets(buckets)
{ }



vector< vector<int> > Water::getNeighbors(const vector<int>& cfg) const
{
	vector< vector<int> > neighbors;

	for(vector<int>::const_iterator iter = _buckets.begin();
			iter != _buckets.end();
			iter++)
	{
		int newval = cfg + *iter;
		if(newval <= _end)
			neighbors.push_back(newval);
	}

	return neighbors;
}	

bool Water::isSolution(const int& cfg) const
{
	return cfg == _end;
}

int Water::getStartConfig() const
{
	return vector<int>(buckets.size(), 0);
}

string Water::stringFromConfig(const int& cfg) const
{
	stringstream s;
	s << cfg;
	return s.str();
}

#endif

/* Water log:
 *
 * $Log: Water.h,v $
 * Revision 1.1  2006/04/24 13:42:57  noah
 *
 *
 * Added water start.
 *
 *
 */
