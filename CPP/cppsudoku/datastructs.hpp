#pragma once
#include <variant>
#include <optional>
#include <vector>
#include <array>
#include <numeric>
#include <cassert>
#include <functional>
#include <range/v3/all.hpp>
#include <algorithm>
#include <set>
constexpr int sqrtsize=3;
constexpr int size=sqrtsize*sqrtsize;


struct CellInput {
  int p,v;
};

struct Pos {
  int row,col;
};

using TIndexes = std::array<int,size*size>;
using TVctInt = std::vector<int>;
using TVctIntOpt = std::vector<std::optional< int>>;

using Candidates = TVctInt;
using Value = std::variant<int,Candidates>;

struct Cell {
  int idx;
  Value val;
};

using Table = std::vector <Cell>;
using TableOpt = std::vector <std::optional<Cell>>;


struct Node {
   Table act ;
   std::vector<Table> contenders;
};

struct OrdCandidates {
  int indx,lng;
};

using VctOrdCandidates=std::vector<OrdCandidates>;

inline  TIndexes indexes()
{
    TIndexes ret;
    std::iota(ret.begin(), ret.end(), 0);
    return ret;
}

struct Neighbours{
       Cell pivot ;
       Table rows ,cols ,block ;
};



struct CandidatIndexes{
             int candidate;
             TVctInt idxs ;//list of indexes where the candidate might be filled
};
using TVctCandidatIndexes= std::vector<CandidatIndexes>;


template <typename T >
decltype(auto) catMaybes(const std::vector<std::optional<T>> &v)
{

    std::vector<T> res;
    for(auto &o:v)
        if(o)
            res.emplace_back(*o);
    return res;
}







