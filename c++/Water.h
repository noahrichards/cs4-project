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

    // First, produce all individual "fills" and "dumps"
    
    for(size_t i = 0; i < cfg.size(); i++)
    {
        if(cfg[i] != _buckets[i])
        {
            vector<int> neighbor(cfg);
            neighbor[i] = _buckets[i];
            neighbors.push_back(neighbor);
        }

        if(cfg[i] != 0)
        {
            vector<int> neighbor(cfg);
            neighbor[i] = 0;
            neighbors.push_back(neighbor);
        }
    }

    // Now, produce all "pours"
    for(size_t from = 0; from < cfg.size(); from++)
    {
        for(size_t to = 0; to < cfg.size(); to++)
        {
            if(from == to)
                continue;

            // The amount we can pour is the smaller of the
            // capacity of the target and whatever is in
            // the source bucket.
            int to_pour = min(_buckets[to] - cfg[to],
                              cfg[from]);

            if (to_pour > 0)
            {
                vector<int> neighbor(cfg);
                neighbor[from] -= to_pour;
                neighbor[to] += to_pour;
                neighbors.push_back(neighbor);
            }
        }
    }

	return neighbors;
}	

bool Water::isSolution(const vector<int>& cfg) const
{
    for(vector<int>::const_iterator iter = cfg.begin();
            iter != cfg.end();
            iter++)
    {
        if(*iter == _end)
            return true;
    }
    
    return false;
}

vector<int> Water::getStartConfig() const
{
	return vector<int>(_buckets.size(), 0);
}

string Water::stringFromConfig(const vector<int>& cfg) const
{
	stringstream s;
    s << "{";

    for(size_t i = 0; i < cfg.size(); i++)
    {
        s << cfg[i] << "/(" << _buckets[i] << ")";

        if (i < cfg.size() - 1)
            s << ", ";
    }
    s << "}";
 
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
