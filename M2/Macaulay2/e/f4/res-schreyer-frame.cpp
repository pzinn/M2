// Copyright 2014 Michael E. Stillman

#include "f4-monlookup.hpp"
#include "res-schreyer-frame.hpp"
#include "../timing.hpp"

#include <iostream>
#include <iomanip>
#include <algorithm>

MonomialCounter::MonomialCounter(const ResMonoid& M)
  : mIgnoreMonomials(new ResMonomialsIgnoringComponent(M)),
    mAllMonomials(mIgnoreMonomials),
    mNumAllMonomials(0),
    mNextMonom(nullptr),
    mMonoid(M)
{
  // start out mNextMonom
  mNextMonom = mMonomSpace.reserve(monoid().max_monomial_size());
}
void MonomialCounter::accountForMonomial(const packed_monomial mon)
{
  // First copy monomial
  // Then call find_or_insert
  // If not there, increment number of monomials
  // If there: intern monomial

  monoid().copy(mon, mNextMonom);
  packed_monomial not_used;
  if (mAllMonomials.find_or_insert(mNextMonom, not_used))
    {
      // true, means that it was already there
      // nothing needs to be done
    }
  else
    {
      // false: new monomial
      mNumAllMonomials++;
      mMonomSpace.intern(monoid().monomial_size(mNextMonom));
      mNextMonom = mMonomSpace.reserve(monoid().max_monomial_size());      
    }
}



namespace {
  class PreElementSorter
  {
  public:
    typedef SchreyerFrameTypes::PreElement* value;
  private:
    static long ncmps;
  public:
    int compare(value a, value b)
    {
      ncmps ++;
      if (a->degree > b->degree) return GT;
      if (a->degree < b->degree) return LT;
      return varpower_monomials::compare(a->vp, b->vp);
    }
    
    bool operator()(value a, value b)
    {
      ncmps ++;
      if (a->degree > b->degree) return false;
      if (a->degree < b->degree) return true;
      return varpower_monomials::compare(a->vp, b->vp) == LT;
    }
    
    PreElementSorter() {}
    
    void reset_ncomparisons() { ncmps = 0; }
    long ncomparisons() const { return ncmps; }
    
    ~PreElementSorter() {}
  };

  long PreElementSorter::ncmps = 0;  
};

SchreyerFrame::SchreyerFrame(const ResPolyRing& R, int max_level)
  : mRing(R),
    mState(Initializing),
    mCurrentLevel(0),
    mSlantedDegree(0),
    mLoSlantedDegree(0),
    mHiSlantedDegree(0),
    mComputer(*this)
    //mAllMonomials(R.monoid())
{
  mFrame.mLevels.resize(max_level+1);
  mMaxVPSize = 2*monoid().n_vars() + 1;

  timeMakeMatrix = 0.0;
  timeSortMatrix = 0.0;
  timeReorderMatrix = 0.0;
  timeGaussMatrix = 0.0;
  timeClearMatrix = 0.0;
  timeResetHashTable = 0.0;
  timeComputeRanks = 0.0;
}
  
// Destruct the frame
SchreyerFrame::~SchreyerFrame() 
{
  // Nothing to do here yet
  // the monomial block will free itself
  // as will the std::vector's
}

void SchreyerFrame::start_computation(StopConditions& stop)
{
  decltype(timer()) timeA, timeB;
  if (level(0).size() == 0)
    mState = Done;;
  while (true)
    {
      switch (mState) {
      case Initializing:
        break;
      case Frame:
        if (M2_gbTrace >= 1)
          std::cout << "maxsize = " << mFrame.mLevels.size() << " and mCurrentLevel = " << mCurrentLevel << std::endl;
        if (mCurrentLevel >= mFrame.mLevels.size() or computeNextLevel() == 0)
          {
            //show(6);
            mState = Matrices;
            mCurrentLevel = 2;
            getBounds(mLoSlantedDegree, mHiSlantedDegree, mMaxLength);
            mSlantedDegree = mLoSlantedDegree;
            setBettiDisplays();
            if (M2_gbTrace >= 1)
              {
                std::cout << "non-minimal betti: " << std::endl;
                mBettiNonminimal.output();
              }
            //for (int i=0; i<mMinimalizeTODO.size(); i++)
            //  {
            //     auto a = mMinimalizeTODO[i];
            //     std::cout << "(" << a.first << "," << a.second << ") ";
            //  }
            // std::cout << std::endl;
          }
        break;
      case Matrices:
        if (M2_gbTrace >= 1)
          std::cout << "start_computation: entering matrices(" << mSlantedDegree << ", " << mCurrentLevel << ")" << std::endl;
        if (stop.always_stop) return;
        if (mCurrentLevel > mMaxLength)
          {
            mCurrentLevel = 2;
            mSlantedDegree++;
            if (mSlantedDegree > mHiSlantedDegree)
              {
                if (M2_gbTrace >= 1)
                  showMemoryUsage();
#if 0                
                debugCheckOrderAll();
#endif
                timeA = timer();
                for (auto it=mMinimalizeTODO.cbegin(); it != mMinimalizeTODO.cend(); ++it)
                  {
                    int rk = rank(it->first, it->second);
                    mBettiMinimal.entry(it->first, it->second) -= rk;
                    mBettiMinimal.entry(it->first+1, it->second-1) -= rk;
                  }
                timeB = timer();
                timeComputeRanks += seconds(timeB-timeA);
                mState = Done;
                if (M2_gbTrace >= 1)
                  mBettiMinimal.output();
                 break;
              }
            if (stop.stop_after_degree and mSlantedDegree > stop.degree_limit->array[0])
              return;
          }
        if (M2_gbTrace >= 2)
          {
            std::cout << "construct(" << mSlantedDegree << ", " << mCurrentLevel << ")..." << std::flush;
          }
        mComputer.construct(mCurrentLevel, mSlantedDegree+mCurrentLevel);
        if (M2_gbTrace >= 2)
          {
            std::cout << "done" << std::endl;
          }
        ///std::cout << "Number of distinct monomials so far = " << mAllMonomials.count() << std::endl;
        mCurrentLevel++;
        break;
      case Done:
        if (M2_gbTrace >= 1)
          {
            std::cout << "total time for make matrix: " << timeMakeMatrix << std::endl;
            std::cout << "total time for sort matrix: " << timeSortMatrix << std::endl;
            std::cout << "total time for reorder matrix: " << timeReorderMatrix << std::endl;
            std::cout << "total time for gauss matrix: " << timeGaussMatrix << std::endl;
            std::cout << "total time for clear matrix: " << timeClearMatrix << std::endl;
            std::cout << "total time for reset hash table: " << timeResetHashTable << std::endl; 
            std::cout << "total time for computing ranks: " << timeComputeRanks << std::endl;
          }
        return;
      default:
        break;
      }
    }
}

M2_arrayint SchreyerFrame::getBetti(int type) const
{
  if (type == 4)
    return mBettiMinimal.getBetti();
  if (type == 0 or type == 1)
    return getBettiFrame();
  
  ERROR("betti display not implemenented yet");
  return 0;
}

void SchreyerFrame::endLevel()
{
  setSchreyerOrder(mCurrentLevel);
  mCurrentLevel++;
  if (mCurrentLevel == 2)
    {
      mState = Frame;
    }
}

SchreyerFrame::PreElement* SchreyerFrame::createQuotientElement(packed_monomial m1, packed_monomial m)
{
  PreElement* vp = mPreElements.allocate();
  vp->vp = mVarpowers.reserve(mMaxVPSize);
  monoid().quotient_as_vp(m1, m, vp->vp);
  vp->degree = monoid().degree_of_vp(vp->vp);
  int len = static_cast<int>(varpower_monomials::length(vp->vp));
  mVarpowers.intern(len);
  return vp;
}
long SchreyerFrame::computeIdealQuotient(int lev, long begin, long elem)
{
  ///  std::cout << "computeIdealQuotient(" << lev << "," << begin << "," << elem << ")" << std::endl;
  // Returns the number of elements added
  packed_monomial m = monomial(lev, elem); 
  std::vector<PreElement*> elements;
  if (ring().isSkewCommutative())
    {
      auto skewvars = new int[ring().monoid().n_vars()];
      int a = ring().monoid().skew_vars(ring().skewInfo(), m, skewvars);
      // std::cout << "adding " << a << " syz from skew" << std::endl;
      for (int i=0; i<a; ++i)
        {
          PreElement* vp = mPreElements.allocate();
          vp->vp = mVarpowers.reserve(mMaxVPSize);
          monoid().variable_as_vp(skewvars[i], vp->vp); 
          vp->degree = monoid().degree_of_vp(vp->vp);         
          int len = static_cast<int>(varpower_monomials::length(vp->vp));
          mVarpowers.intern(len);

          elements.push_back(vp);
        }
      delete [] skewvars;
    }
  for (long i=begin; i<elem; i++)
    elements.push_back(createQuotientElement(monomial(lev,i), m));
  typedef F4MonomialLookupTableT<int32_t> MonomialLookupTable;
  MonomialLookupTable montab(monoid().n_vars());

#if 0
  std::cout << "  #pre elements = " << elements.size() << std::endl;
  for (auto i=elements.begin(); i != elements.end(); ++i)
    {
      varpower_monomials::elem_text_out(stdout, (*i)->vp);
      fprintf(stdout, "\n");
    }
#endif
  PreElementSorter C;
  std::sort(elements.begin(), elements.end(), C);

  long n_elems = 0;
  for (auto i = elements.begin(); i != elements.end(); ++i)
    {
      int32_t not_used;
      bool inideal = montab.find_one_divisor_vp(0, (*i)->vp, not_used);
      if (inideal) continue;
      // Now we create a packed_monomial, and insert it into 'lev+1'
      montab.insert_minimal_vp(0, (*i)->vp, 0);
      packed_monomial monom = monomialBlock().allocate(monoid().max_monomial_size());
      monoid().from_varpower_monomial((*i)->vp, elem, monom);
      // Now insert it into the frame
      insertBasic(currentLevel(), monom, (*i)->degree + degree(currentLevel()-1, monoid().get_component(monom)));
      n_elems++;
    }
  //std::cout << "  returns " << n_elems << std::endl;
  return n_elems;
}

long SchreyerFrame::computeNextLevel()
{
  if (currentLevel() == 1) return 0;
  if (currentLevel() >= mFrame.mLevels.size()) return 0;
  //  std::cout << "computeNextLevel: level = " << currentLevel() << std::endl;
  // loop through all the elements at level currentLevel()-2
  int level0 = currentLevel()-2;
  int level1 = level0+1;
  long n_elems_added = 0;
  for (auto i = level(level0).begin(); i != level(level0).end(); ++i)
    {
      long begin = (*i).mBegin;
      long end = (*i).mEnd;
      for (long i=begin; i<end; ++i)
        {
          auto& elem = level(level1)[i];
          elem.mBegin = n_elems_added;
          n_elems_added += computeIdealQuotient(level1, begin, i);
          elem.mEnd = n_elems_added;
        }
    }
  //show();
  setSchreyerOrder(mCurrentLevel);
  mCurrentLevel++;
  return n_elems_added;
}

void SchreyerFrame::setSchreyerOrder(int lev)
{
  auto& myframe = level(lev);
  auto& myorder = schreyerOrder(lev);
  myorder.mTieBreaker.resize(myframe.size());
  if (lev == 0)
    {
      for (long i=0; i<myorder.mTieBreaker.size(); i++)
        myorder.mTieBreaker[i] = i;
      return;
    }

  auto& prevframe = level(lev-1);
  auto& prevorder = schreyerOrder(lev-1);
  long* tiebreakers = new long[myframe.size()];
  
  for (long i=0; i<myframe.size(); i++)
    {
      long comp = monoid().get_component(myframe[i].mMonom);
      tiebreakers[i] = i + myframe.size() * prevorder.mTieBreaker[comp];
    }
  std::sort(tiebreakers, tiebreakers + myframe.size());

  
  for (long i=0; i<myframe.size(); i++)
    {
      myorder.mTieBreaker[tiebreakers[i] % myframe.size()] = i;
    }
  delete [] tiebreakers;
}

void SchreyerFrame::insertBasic(int lev, packed_monomial monom, int degree)
{
  // if lev >= 2, then level(lev-1)[comp].(mBegin,mEnd) is set separately.
  auto& myframe = level(lev);
  long idx = myframe.size();
  myframe.emplace_back(FrameElement(monom,degree));
  auto& myelem = myframe[idx];

  // The rest of this code simply sets the total monomial for the Schreyer order
  // and should be moved out of here. (MES 3 Feb 2016)
  auto& myorder = schreyerOrder(lev);
  auto myTotalMonom = monomialBlock().allocate(monoid().max_monomial_size());
  if (lev > 0)
    {
      auto& prevlevel = level(lev-1);
      auto& prevorder = schreyerOrder(lev-1);
      long comp = monoid().get_component(myelem.mMonom);
      monoid().unchecked_mult(myelem.mMonom, prevorder.mTotalMonom[comp], myTotalMonom);
      monoid().set_component(monoid().get_component(prevorder.mTotalMonom[comp]), myTotalMonom);
    }
  else
    {
      monoid().copy(myelem.mMonom, myTotalMonom);
    }
  myorder.mTotalMonom.push_back(myTotalMonom);
}

void SchreyerFrame::insertLevelZero(packed_monomial monom, int degree, int maxdeglevel0)
{
  //  return insertBasic(0, monom, degree);

  auto& myframe = level(0);
  long idx = myframe.size();
  myframe.emplace_back(FrameElement(monom,degree));
  auto& myelem = myframe[idx];

  auto& myorder = schreyerOrder(0);
  auto myTotalMonom = monomialBlock().allocate(monoid().max_monomial_size());
  // Create the total monomial.  It is monom * (firstvar)^(maxdeglevel0-degree)
#if 0
  long comp;
  ntuple_monomial exp = new int[monoid().n_vars()];
  to_exponent_vector(monom, exp, comp);
  exp[0] += (maxdeglevel0 - XXXX);
  from_exponent_vector(exp, comp, monom); // XXXX not correct, I think.
#endif
  monoid().copy(myelem.mMonom, myTotalMonom);
  myorder.mTotalMonom.push_back(myTotalMonom);
}
bool SchreyerFrame::insertLevelOne(packed_monomial monom, int deg, poly& syzygy)
{
  insertBasic(1, monom, deg); // deg is the actual degree of this element.
  long comp = monoid().get_component(monom);
  auto last = level(1).size();
  auto& p = level(0)[comp];
  if (p.mBegin == -1)
    p.mBegin = last-1;
  p.mEnd = last;
  if (!check_poly(ring(), syzygy, schreyerOrder(0)))
    {
      if (M2_gbTrace >= 1)
        {
          std::cout << "Error: expected terms of polynomial to be in order, in poly#" << last << ": ";
          display_poly(stdout, ring(), syzygy);
          std::cout << std::endl;
        }
      return false;
    }
  std::swap(level(1)[level(1).size()-1].mSyzygy, syzygy);
  return true;
}
//long SchreyerFrame::insert(packed_monomial monom)
//{
//  return insertBasic(currentLevel(), monom, degree(currentLevel(), monom));
//}

bool SchreyerFrame::debugCheckOrder(int lev) const
{
  if (lev == 0) return true;
  bool result = true;
  auto& mylevel = level(lev);
  auto& myorder = schreyerOrder(lev-1);
  int which = 0;
  for (auto i = mylevel.cbegin(); i != mylevel.cend(); ++i, ++which)
    {
      if (!check_poly(ring(), i->mSyzygy, myorder))
        {
          std::cout << "Error: terms of polynomial at level " << lev << " location " << which << " not in order" << std::endl;
          std::cout << "  poly = ";
          display_poly(stdin, ring(), i->mSyzygy);
          std::cout << std::endl;
          result = false;
        }
    }
  return result;
}
bool SchreyerFrame::debugCheckOrderAll() const
{
  std::cout << "checking that all input and constructed polynomials are in order...";
  bool result = true;
  for (auto i = 1; i<maxLevel(); ++i)
    if (!debugCheckOrder(i))
      result = false;
  if (result)
    std::cout << "ok" << std::endl;
  return result;
}

long SchreyerFrame::memoryUsage() const
{
  long result = mMonomialSpace.memoryUsage();
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      result += level(i).capacity() * sizeof(FrameElement);
    }
  return result;
}

void SchreyerFrame::showMemoryUsage() const
{
  std::cout << "Frame memory usage" << std::endl;
  // widths: level: 6, #elems: 8, used: 6, allocated: 11
  std::cout << " level" << "   #elems" << "   used" << "   allocated" << "     nterms" << "       poly" << "   polalloc" << std::endl;
  long alloc = 0;
  long used = 0;
  long nelems = 0;
  long poly_used = 0;
  long poly_alloc = 0;
  long poly_nterms = 0;
  long poly_used_level = 0;
  long poly_alloc_level = 0;
  long poly_nterms_level = 0;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      long nelems_level = level(i).size();
      if (nelems_level == 0) continue;
      long used_level = nelems_level * sizeof(FrameElement);
      long alloc_level = level(i).capacity() * sizeof(FrameElement);
      poly_nterms_level = 0;
      poly_used_level = 0;
      poly_alloc_level = 0;
      for (int j=0; j<nelems_level; j++)
        {
          ring().memUsage(level(i)[j].mSyzygy, poly_nterms_level, poly_used_level, poly_alloc_level);
        }
      poly_nterms += poly_nterms_level;
      poly_used += poly_used_level;
      poly_alloc += poly_alloc_level;
      std::cout << std::setw(6) << i
                << " " << std::setw(8) << nelems_level
                << " " << std::setw(6) << used_level
                << " " << std::setw(11) << alloc_level
                << " " << std::setw(10) << poly_nterms_level        
                << " " << std::setw(10) << poly_used_level
                << " " << std::setw(10) << poly_alloc_level
                << std::endl;
      nelems += nelems_level;
      used += used_level;
      alloc += alloc_level;
    }
  std::cout << "   all"
            << " " << std::setw(8) << nelems
            << " " << std::setw(6) << used
            << " " << std::setw(11) << alloc
            << " " << std::setw(10) << poly_nterms
            << " " << std::setw(10) << poly_used
            << " " << std::setw(10) << poly_alloc
            << std::endl;

  long monomSpace = mMonomialSpace.memoryUsage();
  long monomUsed = nelems * monoid().max_monomial_size() * sizeof(monomial_word);
  std::cout << "monomials     "
            << std::setw(6) << monomUsed
            << " " << std::setw(11) << monomSpace
            << std::endl;
  std::cout << "total mem     "
            << std::setw(6) << (used+monomUsed+poly_used)
            << " " << std::setw(11) << (alloc+monomSpace+poly_alloc)
            << std::endl;
}

void SchreyerFrame::show(int len) const
{
  std::cout << "#levels=" << mFrame.mLevels.size() << " currentLevel=" << currentLevel() << std::endl;
  for (int i=0; i<mFrame.mLevels.size(); i++)
    {
      auto& myframe = level(i);
      auto& myorder = schreyerOrder(i);
      if (myframe.size() == 0) continue;
      std::cout << "--- level " << i << " ------" << std::endl;
      for (int j=0; j<myframe.size(); j++)
        {
          std::cout << "    " << j << " " << myframe[j].mDegree 
                    << " (" << myframe[j].mBegin << "," << myframe[j].mEnd << ") " << std::flush;
          std::cout << "(size:" << myframe[j].mSyzygy.len << ") [";
          monoid().showAlpha(myorder.mTotalMonom[j]);
          std::cout << "  " << myorder.mTieBreaker[j] << "] ";
          if (len == 0 or myframe[j].mSyzygy.len == 0)
            monoid().showAlpha(myframe[j].mMonom);
          else
            display_poly(stdout, ring(), myframe[j].mSyzygy);
          std::cout << std::endl;
        }
    }
  showMemoryUsage();
}

void SchreyerFrame::getBounds(int& loDegree, int& hiDegree, int& length) const
{
  auto lev0 = level(0);
  loDegree = hiDegree = static_cast<int>(lev0[0].mDegree);
  for (int lev=0; lev<mFrame.mLevels.size(); lev++)
    {
      auto& myframe = level(lev);
      if (myframe.size() == 0) return;
      length = lev;
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree;
          deg -= lev; // slanted degree
          if (deg < loDegree) loDegree = deg;
          if (deg > hiDegree) hiDegree = deg;
        }
    }
  //  show();
}

void SchreyerFrame::setBettiDisplays()
{
  int lo, hi, len;
  getBounds(lo, hi, len);
  //std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len << std::endl;
  mBettiNonminimal = BettiDisplay(lo,hi,len);
  mBettiMinimal = BettiDisplay(lo,hi,len);

  for (int lev=0; lev<=len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree; // this is actual degree, not slanted degree
          mBettiNonminimal.entry(deg-lev,lev) ++ ;
          mBettiMinimal.entry(deg-lev,lev) ++ ;
        }
    }

  // Now set the todo list of pairs (degree, level) for minimalization.
  for (int slanted_degree = lo; slanted_degree < hi; slanted_degree++)
    {
      for (int lev = 1; lev <= len; lev++)
        {
          if (mBettiNonminimal.entry(slanted_degree, lev) > 0 and mBettiNonminimal.entry(slanted_degree+1, lev-1) > 0)
            {
              mMinimalizeTODO.push_back(std::make_pair(slanted_degree, lev));
            }
        }
    }
}


M2_arrayint SchreyerFrame::getBettiFrame() const
{
  int lo, hi, len;
  getBounds(lo, hi, len);
  //  std::cout << "bounds: lo=" << lo << " hi=" << hi << " len=" << len << std::endl;
  BettiDisplay B(lo,hi,len);
  // now set B

  for (int lev=0; lev<=len; lev++)
    {
      auto& myframe = level(lev);
      for (auto p=myframe.begin(); p != myframe.end(); ++p)
        {
          int deg = p->mDegree; // this is actual degree, not slanted degree
          B.entry(deg-lev,lev) ++ ;
        }
    }

  return B.getBetti();
}


// local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
