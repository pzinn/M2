// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_REDUCER_HASH_PACK_GUARD
#define MATHICGB_REDUCER_HASH_PACK_GUARD

MATHICGB_NAMESPACE_BEGIN

// This translation unit has to expose something that is needed elsewhere.
// Otherwise, the compiler will think it is not needed and exclude the
// whole thing, despite there being important global objects in the .cpp file.
void reducerHashPackDependency();

MATHICGB_NAMESPACE_END

#endif
