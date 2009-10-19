/*
 * Slide.h
 *
 * This header contains the declaration of the slide puzzle class.
 * This game is equivalent to <gnome-game>  
 *
 * Author: Noah Richards
 *
 * $Id: Slide.h,v 1.4 2006/04/24 13:42:57 noah Exp $
 */


#ifndef SLIDE_H
#define SLIDE_H

#include <vector>
#include <iostream>
#include <sstream>
#include <list>
#include "Puzzle.h"
using namespace std;

class Slide;

struct Board
{
	// Pointer to Slide puzzle that holds
	// information about board
	Slide* _puzzle;

	// Board representation: matrix of ints where
	// -1 denotes an empty square and any other
	// integer >= 0 denotes a block
	vector<int> _board;

	// Canonicalized form of the board.  This is generated
	// the first time it is needed and referred to
	// afterwards
	mutable vector<int> _canon;
	mutable bool _beenCanonicalized;
	
	Board() : _puzzle(0), _beenCanonicalized(false) {} ;

	Board(Slide* puzzle) : _puzzle(puzzle), _beenCanonicalized(false) {} ;

	Board(const Board& rhs) : _puzzle(rhs._puzzle), _board(rhs._board),
							  _canon(), _beenCanonicalized(false) {};

	Board& operator=(const Board& rhs)
	{
		_beenCanonicalized = false;
		_puzzle = rhs._puzzle;
		_board = rhs._board;
		return *this;
	}

	bool operator<(const Board& rhs) const;

	bool operator==(const Board& rhs) const;

	bool operator!=(const Board& rhs) const;

	void canonicalize() const;
};



class Slide : public Puzzle<Board>
{
public:
	friend struct Board;
	typedef std::pair<unsigned int, unsigned int> block_info;
	typedef Board config_type;

	// Exceptions
	class StreamException {};
	class InvalidStreamException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream passed to constructor is not valid.\n";
		}
	};

	class InvalidDataException : public std::exception, public StreamException
	{
		const char* what() const throw()
		{
			return "Input stream data is invalid.\n";
		}
	};

	Slide(istream& in);
	Slide(const char* filename);

	vector<config_type> getNeighbors(const config_type&) const;
	bool isSolution(const config_type&) const;
	config_type getStartConfig() const;
	string stringFromConfig(const config_type&) const;

private:

	void processFile(istream& in);

	inline int intFromRC(int r, int c) const
	{
		return r * _cols + c;
	}

	inline bool moveBlock(config_type& cfg, int row, int col,
		   int targetrow, int targetcol)  const
	{
		int startindex = intFromRC(row, col);
		int blocknum = cfg._board[startindex];
		int height = _blockinfo[blocknum].first;
		int width = _blockinfo[blocknum].second;

		for(int r = row; r < row + height; r++)
		{
			for(int c = col; c < col + width; c++)
			{
				cfg._board[intFromRC(r, c)] = -1;
			}
		}

		for(int r = targetrow; r < targetrow + height; r++)
		{
			for(int c = targetcol; c < targetcol + width; c++)
			{
				int ix = intFromRC(r, c);
				if(cfg._board[ix] != -1)
					return false;
				cfg._board[ix] = blocknum;
			}
		}
		
		return true;
	}

	unsigned int _rows, _cols, _numBlocks;
	unsigned int _targetrow, _targetcol;
	config_type _start;
	vector<block_info> _blockinfo;
	vector<int> _blockids;

};

#endif

/* Change log:
 *
 * $Log: Slide.h,v $
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
 * Revision 1.2  2006/02/08 05:24:18  noah
 * Explicitly defined operator= and Board(const Board&) so that
 * the new _beenCanonized bool is always set to false by
 * default.  Added _canon and _beenCanonized to remember
 * the canonized representation (instead of generating it
 * every time it is needed).
 *
 * Revision 1.1  2006/01/28 05:12:49  noah
 * Initial revision
 */
