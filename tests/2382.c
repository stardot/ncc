/*
 * ARM C compiler regression test $RCSfile$
 * Copyright (C) 1995 Advanced Risc Machines Ltd. All rights reserved.
 * SPDX-Licence-Identifier: Apache-2.0
 */

/*
 * RCS $Revision$
 * Checkin $Date$
 * Revising $Author$
 */

#include <stdio.h>
#include "testutil.h"


#ifdef __cplusplus

   typedef unsigned int size_t;
                class SingleObject {};
        typedef unsigned char   Boolean;
typedef char                    Char;
typedef signed char             SChar;
typedef unsigned char   UChar;
        typedef signed char             Byte;
typedef signed char             SByte;
typedef unsigned char   UByte;
typedef short                   Short;
typedef signed short    SShort;
typedef unsigned short  UShort;
typedef long                    Long;
typedef signed long             SLong;
typedef unsigned long   ULong;
typedef signed long             FastInt;


typedef long Ref;

class RefVar {
  public:
        RefHandle* h;
        inline  RefVar();
        inline  ~RefVar();
        inline  RefVar(const Ref r);
        inline  RefVar(const RefVar& o);
        operator long() const                           { return h->ref; }
        inline  long    Length()                                                                                        const;
};

struct ObjectTable {
        ObjHeader       header;
        RefHandle       handles[1];
};
class TObjectHeap {
  protected:
        Ptr                             fMemory;
        Ptr                             fStart;
        Ptr                             fLimit;
        ObjectTable*    fObjectTable;
        long                    fFreeOTEntry;
        void            Uriah(void);
};


class  POutTranslator
{
public:
        int Print(const char *fmt, ...);
};
extern POutTranslator* gREPout;


void    TObjectHeap::Uriah()
{
        long totalSize = 0;
        long freeSize = 0;
        long lockedSize = 0;
        long largestFree = 0;
        ULong internalFrag = 0;
        long totalMapSize = 0;
        long virtualMapSize = 0;
        long totalSymbolSize = 0;
        long nCodeBlocks = 0;
        long totalCodeSize = 0;
        long totalLitSize = 0;
        long virtualLitSize = 0;
        long totalScriptSize = 0;
        long totalBinarySize = 0;
        long totalBinarys = 0;
        long totalArraySize = 0;
        long totalArrays = 0;
        long totalFrameSize = 0;
        long totalFrames = 0;
        long totalContextSize = 0;
        Ptr start = fStart;
        Ptr limit = fLimit;

        for (Ptr obj = start; obj < limit; obj += ObjPhysicalSize(obj)) {
                long physSize = ObjPhysicalSize(obj);
                totalSize += physSize;
                if ( ( kObjFree) ) {
                        freeSize += physSize;
                        if (physSize > largestFree) largestFree = physSize;
                }
                else {
                        internalFrag += physSize -  ((long) ((*(unsigned long *) obj ) >> 8)) ;
                                lockedSize += physSize;
                        Ref oref =  ((Ref) (((long)  obj ) + kTagPointer)) ;
                        Ref c = NILREF;
                                totalSymbolSize += physSize;
            {
                                Ref instrs = GetFrameSlot(oref,  (RSSYMinstructions) );
                                Ref lits = GetFrameSlot(oref,  (RSSYMliterals) );
                                        nCodeBlocks++;
                                        totalCodeSize += ObjPhysicalSize(ObjectPtr(instrs));
                                                virtualLitSize += 4 * Length(lits);
                                        totalScriptSize += physSize;
                        }
                                totalLitSize += physSize;
                        }
                }

          gREPout->Print  ("total %d, free %d, largest %d, locked %d, int frag %d,
ext frag %d\n",
                        totalSize, freeSize, largestFree, lockedSize,
                        (internalFrag * 1000) / totalSize,
                        ((freeSize - largestFree) * 1000) / totalSize);
          gREPout->Print  ("%d scripts: %d bytecode, p:%d/v:%d literals, grand
total %d\n",
                        nCodeBlocks, totalScriptSize, totalLitSize, virtualLitSize,
                                totalCodeSize+totalScriptSize+totalLitSize);
          gREPout->Print  ("frames %d(%d)maps p:%d/v:%d\n", totalFrameSize,
totalFrames, totalMapSize, virtualMapSize);
          gREPout->Print  ("symbols %d, binaries %d(%d)\n", totalSymbolSize,
totalBinarySize, totalBinarys);
          gREPout->Print  ("*** total size should be %X\n", fLimit - fStart);
        long nextFreeIndex = fFreeOTEntry;
        while (nextFreeIndex != -1)
                {
                nextFreeIndex = RINT(fObjectTable->handles[nextFreeIndex].ref);
                }
}

#endif

/* no runnable test */

int main(void)
{
    BeginTest();
    EQI(0, 0);
    EndTest();
    return 0;
}


