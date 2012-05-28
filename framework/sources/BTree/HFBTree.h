//
//  HFBTree.h
//  HexFiend
//
//

#import <Foundation/Foundation.h>

typedef unsigned long long HFBTreeIndex;

#if ! NDEBUG
#define HFTEST_BTREES 1
#endif

// Insert at a given offset
// Find node containing offset
// Remove entry at a given offset

// We have an annotation: length
// Walk nodes until we find the node at the given offset

@interface HFBTree : NSObject <NSMutableCopying> {
    const struct HFBTreeCallbacks_t *callbacks;
    unsigned int depth;
    int role;
    struct HFBTreeNode *root;
}

- (id)init;
- (void)insertEntry:(id)entry atOffset:(HFBTreeIndex)offset;

- (id)entryContainingOffset:(HFBTreeIndex)offset beginningOffset:(HFBTreeIndex *)outBeginningOffset;
- (void)removeEntryAtOffset:(HFBTreeIndex)offset;
- (void)removeAllEntries;

#if HFTEST_BTREES
- (void)checkIntegrityOfCachedLengths;
- (void)checkIntegrityOfBTreeStructure;
#endif

- (NSEnumerator *)entryEnumerator;
- (NSArray *)allEntries;

- (HFBTreeIndex)length;

/* Applies the given function to the entry at the given offset, continuing with subsequent entries until the function returns NO.  Do not modify the tree from within this function. */
- (void)applyFunction:(BOOL (*)(id entry, HFBTreeIndex offset, void *userInfo))func toEntriesStartingAtOffset:(HFBTreeIndex)offset withUserInfo:(void *)userInfo;

@end

@protocol HFBTreeEntry <NSObject>
- (unsigned long long)length;
@end
