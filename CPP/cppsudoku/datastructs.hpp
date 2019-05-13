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
constexpr int sizeTbl=sqrtsize*sqrtsize;


struct CellInput {
  int p,v;
};

struct Pos {
  int row,col;
};


using TVctInt = std::vector<int>;
using TVctIntOpt = std::vector<std::optional< int>>;

using Candidates = TVctInt;
using Value = std::variant<int,Candidates>;

struct Cell {
  int idx;
  Value val;
};

inline bool operator == (const Cell &a , const Cell &b)
{
    return std::tie(a.idx,a.val)== std::tie(b.idx,b.val);
}



using Grid = std::vector <Cell>;
using GridOpt = std::vector <std::optional<Cell>>;

inline bool operator == (const Grid &a , const Grid &b)
{

    if (a.size()==b.size())
    {
        for(auto i=0u; i< a.size();i++)
        {
            if (!(a[i]==b[i]))
                return false;
        }
    }
    else
       return false;

    return true;

}
struct Node {
   Grid act ;
   std::vector<Grid> contenders;
   std::vector<Grid> solutions;
};

struct OrdCandidates {
  int indx,lng;
};

using VctOrdCandidates=std::vector<OrdCandidates>;

inline  TVctInt indexes()
{
    auto c = ranges::view::take(ranges::view::ints(0), sizeTbl*sizeTbl);
    return TVctInt  {c};
}

struct Neighbours{
       Cell pivot ;
       Grid rows ,cols ,block ;
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







