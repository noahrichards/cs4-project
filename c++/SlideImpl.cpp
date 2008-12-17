/*
 * Slide.cpp
 *
 * This header contains the definition of the Slide class.
 *
 * Author: Noah Richards
 *
 * $Id: Slide.cpp,v 1.3 2006/04/24 13:42:57 noah Exp $
 */

#include "Slide.h"
#include <map>
#include <vector>
#include <fstream>
using namespace std;

Slide::Slide(istream& in) :
	_rows(0), _cols(0), _numBlocks(0), _targetrow(0), _targetcol(0),
	_start(this)
{
	processFile(in);
}

Slide::Slide(const char* filename)
{
	ifstream file(filename);
	processFile(file);
}

void
Slide::processFile(istream& in)
{
	if(!in)
		throw InvalidStreamException();

	in >> _cols >> _rows >> _numBlocks;

	_start._board = vector<int>(_rows * _cols, -1);
	_blockinfo.resize(_numBlocks);
	_blockids.resize(_numBlocks);
	map<block_info, int> id_map;
	int currid = -1;

	for(unsigned int i = 0; i < _numBlocks; i++)
	{
		unsigned int topx, topy, botx, boty;
		in >> topy >> topx >> boty >> botx;

		_blockinfo[i] = block_info(botx - topx, boty - topy);

		if(i == _numBlocks - 1)
			_blockids[i] = ++currid;
		else
		{
			map<block_info, int>::iterator iter = id_map.find(_blockinfo[i]);
			if(iter == id_map.end())
			{
				_blockids[i] = ++currid;
				id_map[_blockinfo[i]] = currid;
			}
			else
				_blockids[i] = iter->second;
		}

		for(unsigned int x = topx; x < botx; x++)
		{
			for(unsigned int y = topy; y < boty; y++)
			{
				_start._board[intFromRC(x, y)] = i;
			}
		}
	}

	in >> _targetcol >> _targetrow;
		
	if(!in)
		throw InvalidDataException();
}



vector<Slide::config_type> Slide::getNeighbors(const config_type& cfg) const
{
	vector<bool> seen(_numBlocks, false);
	vector<config_type> neighbors;

	// For each 0 in the vector, move it around
	for(unsigned int r = 0; r < _rows; r++)
	{
		for(unsigned int c = 0; c < _cols; c++)
		{
			int index = intFromRC(r, c);

			if(cfg._board[index] == -1) continue;
			if(seen[cfg._board[index]]) continue;

			seen[cfg._board[index]] = true;

			// Move up
			if(r != 0)
			{
				config_type next = cfg;
				if(moveBlock(next, r, c, r - 1, c))
				   neighbors.push_back(next);	
			}

			// Move down
			if(r != _rows - _blockinfo[cfg._board[index]].first)
			{
				config_type next = cfg;
				if(moveBlock(next, r, c, r + 1, c))
				   neighbors.push_back(next);	
			}

			// Move left
			if(c != 0)
			{
				config_type next = cfg;
				if(moveBlock(next, r, c, r , c - 1))
				   neighbors.push_back(next);	
			}

			// Move right
			if(c != _cols - _blockinfo[cfg._board[index]].second)
			{
				config_type next = cfg;
				if(moveBlock(next, r, c, r , c + 1))
				   neighbors.push_back(next);	
			}
		}
	}
	return neighbors;
}	

bool Slide::isSolution(const config_type& cfg) const
{
	for(unsigned int r = 0; r < _rows; r++)
	{
		for(unsigned int c = 0; c < _cols; c++)
		{
			int ix = intFromRC(r, c);
			if(r == _targetrow && c == _targetcol)
			{
				if(cfg._board[ix] != (int)_numBlocks - 1)
					return false;
				return true;
			}
			else if(cfg._board[ix] == (int)_numBlocks - 1)
				return false;
			else if(ix > intFromRC(_targetrow, _targetcol))
				return false;
		}
	}
	return false;
}

Slide::config_type Slide::getStartConfig() const
{
	return _start;
}

string Slide::stringFromConfig(const config_type& cfg) const
{
	stringstream s;
	for(unsigned int r = 0; r < _rows; r++)
	{
		for(unsigned int c = 0; c < _cols; c++)
		{
			int val = cfg._board[intFromRC(r, c)];
			if(val == -1)
				s << "X";
			else
				s << val;
		}
		s << "\n";
	}
	return s.str();
}

/* Change log:
 *
 * $Log: Slide.cpp,v $
 * Revision 1.3  2006/04/24 13:42:57  noah
 *
 *
 * Added water start.
 *
 * Revision 1.2  2006/04/10 15:50:42  noah
 *
 * Replaced some map logic in solver.
 *
 * Original: if(!map.find) insert ...
 * Now: if(!insert) ...
 *
 * Revision 1.1.1.1  2006/03/27 00:52:30  noah
 * CS4 Project
 *
 * Revision 1.2  2006/01/29 03:19:10  noah
 * Correct command line argument usage (filenam or "-")
 *
 * Revision 1.1  2006/01/28 05:12:49  noah
 * Initial revision
 */
