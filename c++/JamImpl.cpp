/*
 * Jam.cpp
 *
 * This header contains the definition of the Jam class.
 *
 * Author: Noah Richards
 *
 * $Id: Jam.cpp,v 1.3 2006/04/10 15:50:42 noah Exp $
 */

#include "Jam.h"
#include <map>
#include <vector>
using namespace std;

Jam::Jam(istream& in) :
	_rows(0), _cols(0), _numBlocks(0), _targetrow(0)
{
	if(!in)
		throw InvalidStreamException();

	in >> _cols >> _rows >> _numBlocks;

	_start.resize(_rows * _cols, -1);
	_blockinfo.resize(_numBlocks);

	for(unsigned int i = 0; i < _numBlocks; i++)
	{
		unsigned int topx, topy, botx, boty;
		in >> topx >> topy >> botx >> boty;

		unsigned int width = botx - topx + 1;
		unsigned int height = boty - topy + 1;

		if(width == 1)
			_blockinfo[i] = block_info(height, VERTICAL);
		else
			_blockinfo[i] = block_info(width, HORIZONTAL);

		if(i == _numBlocks - 1)
			_targetrow = topy;

		for(unsigned int r = topy; r <= boty; r++)
			for(unsigned int c = topx; c <= botx; c++)
				_start[intFromRC(r, c)] = i;
	}

		
	if(!in)
		throw InvalidDataException();
}



vector<Jam::config_type> Jam::getNeighbors(const config_type& cfg) const
{
	vector<bool> seen(_numBlocks, false);
	vector<config_type> neighbors;

	// For each 0 in the vector, move it around
	for(unsigned int r = 0; r < _rows; r++)
	{
		for(unsigned int c = 0; c < _cols; c++)
		{
			int index = intFromRC(r, c);

			if(cfg[index] == -1) continue;
			if(seen[cfg[index]]) continue;

			seen[cfg[index]] = true;

			unsigned int size = _blockinfo[cfg[index]].first;
			
			switch(_blockinfo[cfg[index]].second)
			{
			case VERTICAL:
				// Move down
				if(r + size < _rows)
				{
					config_type next = cfg;
					if(moveBlock(next, r, c, r + size, c))
						neighbors.push_back(next);
				}
				// Move up
				if(r > 0)
				{
					config_type next = cfg;
					if(moveBlock(next, r + size - 1, c, r - 1, c))
						neighbors.push_back(next);
				}
				break;
			case HORIZONTAL:
				// Move right
				if(c + size < _cols)
				{
					config_type next = cfg;
					if(moveBlock(next, r, c, r, c + size))
						neighbors.push_back(next);
				}
				// Move left
				if(c > 0)
				{
					config_type next = cfg;
					if(moveBlock(next, r, c + size - 1, r, c - 1))
						neighbors.push_back(next);
				}
				break;
			}
		}
	}
	return neighbors;
}	

bool Jam::isSolution(const config_type& cfg) const
{
	return cfg[intFromRC(_targetrow, _cols - 1)] == (int)_numBlocks - 1;
}

Jam::config_type Jam::getStartConfig() const
{
	return _start;
}

string Jam::stringFromConfig(const config_type& cfg) const
{
	stringstream s;
	for(unsigned int r = 0; r < _rows; r++)
	{
		for(unsigned int c = 0; c < _cols; c++)
		{
			int val = cfg[intFromRC(r, c)];
			if(val == -1)
				s << "X";
			else
				s << char('A' + val);
		}
		s << "\n";
	}
	return s.str();
}

/* Change log:
 *
 * $Log: Jam.cpp,v $
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
 */
