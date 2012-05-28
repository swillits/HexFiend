//
//  HFBTree.m
//  BTree
//
//  Created by peter on 2/6/09.
//  Copyright 2009 ridiculous_fish. All rights reserved.
//

#import "HFBTree.h"
#import <HexFiend/HFFunctions.h>
#include <malloc/malloc.h>

#define FIXUP_LENGTHS 0

#define BTREE_BRANCH_ORDER 10
#define BTREE_LEAF_ORDER 10

#define BTREE_ORDER 10
#define BTREE_NODE_MINIMUM_VALUE_COUNT (BTREE_ORDER / 2)

#define BTREE_LEAF_MINIMUM_VALUE_COUNT (BTREE_LEAF_ORDER / 2)

#define BAD_INDEX ((ChildIndex_t)(-1))
typedef unsigned int ChildIndex_t;

/* How deep can our tree get?  128 is huge. */
#define MAX_DEPTH 128
#define BAD_DEPTH ((TreeDepth_t)(-1))
typedef unsigned int TreeDepth_t;

/* The type of the actual nodes we store. These are CFTypeRefs. */
typedef void TreeEntry_t;

#define HFBTreeLength(x) [(__bridge NSObject <HFBTreeEntry> *)(x) length]

#define GET_LENGTH(node, parentIsLeaf) ((parentIsLeaf) ? HFBTreeLength(node) : ((HFBTreeNode *)node)->subtreeLength)

typedef struct HFBTreeNode HFBTreeNode;

static TreeEntry_t *btree_search(HFBTree *tree, HFBTreeIndex offset, HFBTreeIndex *outBeginningOffset);
static HFBTreeNode *btree_insert_returning_value_for_parent(HFBTree *tree, TreeEntry_t *entry, HFBTreeIndex offset);
static BOOL btree_remove(HFBTree *tree, HFBTreeIndex offset, TreeEntry_t **outRemovedNode);
static void btree_recursive_check_integrity(HFBTree *tree, HFBTreeNode *branchOrLeaf, TreeDepth_t depth, HFBTreeNode **linkHelper);
static void btree_fixup_cached_annotations(HFBTree *tree);
static void btree_check_cached_annotations(HFBTree *tree);
static BOOL btree_are_cached_lengths_correct(HFBTreeNode *branchOrLeaf, TreeDepth_t depth, HFBTreeIndex *outLength);
static NSUInteger btree_entry_count(HFBTree *tree);
static ChildIndex_t count_node_values(HFBTreeNode *node);
static HFBTreeIndex sum_child_lengths(void * const *children, const BOOL isLeaf);
static HFBTreeNode *mutable_copy_node(HFBTreeNode *node, TreeDepth_t depth, HFBTreeNode **linkingHelper);

static void walk_tree_nodes(HFBTree *tree, void (^blk)(HFBTreeNode *node, BOOL isLeaf));

#if NDEBUG
#define VERIFY_LENGTH(a)
#else
#define VERIFY_LENGTH(a) btree_check_cached_annotations((a))
#endif


#define IS_BRANCH(a) [(a) isKindOfClass:[HFBTreeBranch class]]
#define IS_LEAF(a) [(a) isKindOfClass:[HFBTreeLeaf class]]

#if HFTEST_BTREES

#define ASSERT_IS_BRANCH(a) HFASSERT(! a->isLeaf)
#define ASSERT_IS_LEAF(a) HFASSERT(a->isLeaf)

#endif

#define DEFEAT_INLINE 1

#if DEFEAT_INLINE
#define FORCE_STATIC_INLINE static
#else
#define FORCE_STATIC_INLINE static __inline__ __attribute__((always_inline))
#endif

#if 0
@interface HFBTreeEnumerator : NSEnumerator {
    HFBTreeLeaf *currentLeaf;
    ChildIndex_t childIndex;
}

- (id)initWithLeaf:(HFBTreeLeaf *)leaf;

@end
#endif

struct HFBTreeNode {
#if HFTEST_BTREES
    BOOL isLeaf;
#endif
    unsigned long long subtreeLength;
    struct HFBTreeNode *left, *right;
    void *children[BTREE_ORDER];
};

typedef struct HFBTreeNode HFBTreeBranch;
typedef struct HFBTreeNode HFBTreeLeaf;

@implementation HFBTree

/* Descend a tree, applying a block to each node. Recurses before applying the block, so a node's children are processed before the node. */
static void walk_tree_nodes_recursive(HFBTree *tree, HFBTreeNode *node, TreeDepth_t depth, void (^blk)(HFBTreeNode *node, BOOL isLeaf)) {
    HFASSERT(node != NULL);
    HFASSERT(depth >= 0);
    BOOL isLeaf = (depth == 0);
#if HFTEST_BTREES
    HFASSERT(node->isLeaf == isLeaf);
#endif

    /* If we're a branch, then recurse */
    if (! isLeaf) {
        HFBTreeBranch *branch = node;
        for (ChildIndex_t i = 0; i < BTREE_BRANCH_ORDER; i++) {
            if (! branch->children[i]) break;
            walk_tree_nodes_recursive(tree, branch->children[i], depth - 1, blk);
        }
    }
    
    /* Apply the block after */
    blk(node, isLeaf);
}


/* Apply a block to each node in the tree. Recurses before applying the block, so a node's children are processed before the node. */
static void walk_tree_nodes(HFBTree *tree, void (^blk)(HFBTreeNode *node, BOOL isLeaf)) {
    if (tree->root) {
        walk_tree_nodes_recursive(tree, tree->root, tree->depth, blk);
    }
}

/* Allocate a new branch or leaf, zero-filled */
static struct HFBTreeNode *HFBTreeNode_allocate(BOOL isLeaf) {
    struct HFBTreeNode *result = calloc(sizeof *result, 1);
#if HFTEST_BTREES
    result->isLeaf = isLeaf;
#endif
    return result;
}

static void HFBTreeNode_free(HFBTreeNode *node) {
    free(node);
}

/* Release all contained objects, and free all nodes */
void HFBTreeNode_dealloc(HFBTreeNode *node, unsigned int depth) {
    HFASSERT(depth >= 0);
    for (ChildIndex_t i=0; i < BTREE_BRANCH_ORDER; i++) {
        if (! node->children[i]) {
            break;
        }
        if (depth == 0) {
            /* Leaf; release the contained entry, balancing our initial retain on it */
            CFRelease(node->children[i]);
        } else {
            /* Branch, call us recursively */
            HFBTreeNode_dealloc(node->children[i], depth - 1);
        }
    }
    /* Free the actual node */
    HFBTreeNode_free(node);
}

- (id)init {
    self = [super init];
    depth = BAD_DEPTH;
    root = nil;
    return self;
}

- (void)dealloc {
    if (depth != BAD_DEPTH) {
        HFASSERT(root != NULL);
        HFBTreeNode_dealloc(root, depth);
    }
}

#if HFTEST_BTREES
- (void)checkIntegrityOfCachedLengths {
    btree_check_cached_annotations(self);
}

- (void)checkIntegrityOfBTreeStructure {
    if (depth == BAD_DEPTH) {
        HFASSERT(root == nil);
    }
    else {
        HFBTreeNode *linkHelper[MAX_DEPTH + 1] = {};
        btree_recursive_check_integrity(self, root, depth, linkHelper);
    }
}
#endif

- (HFBTreeIndex)length {
    if (root == nil) return 0;
    return root->subtreeLength;
}

- (void)insertEntry:(id)entryObj atOffset:(HFBTreeIndex)offset {
    TreeEntry_t *entry = (void *)CFBridgingRetain(entryObj);
    HFASSERT(entry);
    if (! root) {
        /* Create a single leaf as our root */
        HFASSERT(depth == BAD_DEPTH);
        HFBTreeLeaf *leaf = HFBTreeNode_allocate(YES);
        leaf->children[0] = entry;
        leaf->subtreeLength = HFBTreeLength(entry);
        root = leaf;
        depth = 0;
    }
    else {
        HFBTreeNode *newParentValue = btree_insert_returning_value_for_parent(self, entry, offset);
        if (newParentValue) {
            HFBTreeBranch *newRoot = HFBTreeNode_allocate(NO);
            newRoot->children[0] = root;
            newRoot->children[1] = newParentValue;
            newRoot->subtreeLength = HFSum(root->subtreeLength, newParentValue->subtreeLength);
            root = newRoot;
            depth++;
            HFASSERT(depth <= MAX_DEPTH);
        }
#if FIXUP_LENGTHS
        HFBTreeIndex outLength = -1;
        if (! btree_are_cached_lengths_correct(root, depth, &outLength)) {
            puts("Fixed up length after insertion");
            btree_fixup_cached_annotations(self);
        }
#endif
    }
}

- (id)entryContainingOffset:(HFBTreeIndex)offset beginningOffset:(HFBTreeIndex *)outBeginningOffset {
    HFASSERT(root != nil);
    TreeEntry_t *entry = btree_search(self, offset, outBeginningOffset);
    /* entry is an object of type id that we own a retain on; just return it */
    return (__bridge id)entry;
}

- (void)removeAllEntries {
    if (depth != BAD_DEPTH) {
        HFASSERT(root != NULL);
        HFBTreeNode_dealloc(root, depth);
        root = nil;
        depth = BAD_DEPTH;
    }
}

- (void)removeEntryAtOffset:(HFBTreeIndex)offset {
    HFASSERT(root != nil);
#if FIXUP_LENGTHS
    const NSUInteger beforeCount = btree_entry_count(root);
#endif
    TreeEntry_t *removedEntry = NULL;
    BOOL deleteRoot = btree_remove(self, offset, &removedEntry);
    if (deleteRoot) {
        HFASSERT(count_node_values(root) <= 1);
        HFBTreeNode *newRoot = root->children[0]; //may be nil!
        free(root);
        root = newRoot;
        depth--;
    }
    
    /* Balance our initial CFRetain on the entry */
    if (removedEntry) CFRelease(removedEntry);
    
#if FIXUP_LENGTHS
    const NSUInteger afterCount = btree_entry_count(root);
    if (beforeCount != afterCount + 1) {
        NSLog(@"Bad counts: before %lu, after %lu", beforeCount, afterCount);
    }
    HFBTreeIndex outLength = -1;
    static NSUInteger fixupCount;
    if (! btree_are_cached_lengths_correct(root, depth, &outLength)) {
        fixupCount++;
        printf("Fixed up length after deletion (%lu)\n", (unsigned long)fixupCount);
        btree_fixup_cached_annotations(self);
    }
    else {
        //printf("Length post-deletion was OK! (%lu)\n", fixupCount);
    }
#endif
}

- (id)mutableCopyWithZone:(NSZone *)zone {
    USE(zone);
    HFBTree *result = [[[self class] alloc] init];
    result->depth = depth;
    HFBTreeNode *linkingHelper[MAX_DEPTH + 1];
    bzero(linkingHelper, (1 + depth) * sizeof *linkingHelper);
    result->root = mutable_copy_node(root, depth, linkingHelper);
    return result;
}

FORCE_STATIC_INLINE ChildIndex_t count_node_values(HFBTreeNode *node) {
    ChildIndex_t count;
    for (count=0; count < BTREE_LEAF_ORDER; count++) {
        if (node->children[count] == nil) break;
    }
    return count;
}

FORCE_STATIC_INLINE HFBTreeIndex sum_child_lengths(void * const * children, const BOOL isLeaf) {
    HFBTreeIndex result = 0;
    for (ChildIndex_t childIndex = 0; childIndex < BTREE_ORDER; childIndex++) {
        void *child = children[childIndex];
        if (! child) break;
        HFBTreeIndex childLength = GET_LENGTH(child, isLeaf);
        result = HFSum(result, childLength);
    }
    return result;
}

FORCE_STATIC_INLINE HFBTreeIndex sum_N_child_lengths(void * const *children, ChildIndex_t numChildren, const BOOL isLeaf) {
    HFBTreeIndex result = 0;
    for (ChildIndex_t childIndex = 0; childIndex < numChildren; childIndex++) {
        void *child = children[childIndex];
        HFASSERT(child != NULL);
        HFBTreeIndex childLength = GET_LENGTH(child, isLeaf);
        result = HFSum(result, childLength);
    }
    return result;
}

FORCE_STATIC_INLINE ChildIndex_t index_containing_offset(HFBTreeNode *node, HFBTreeIndex offset, HFBTreeIndex * restrict outOffset, const BOOL isLeaf) {
    ChildIndex_t childIndex;
    HFBTreeIndex previousSum = 0;
    void * const *children = node->children;
    for (childIndex = 0; childIndex < BTREE_ORDER; childIndex++) {
        HFASSERT(children[childIndex] != nil);
        HFBTreeIndex childLength = GET_LENGTH(children[childIndex], isLeaf);
        HFBTreeIndex newSum = HFSum(childLength, previousSum);
        if (newSum > offset) {
            break;
        }
        previousSum = newSum;
    }
    *outOffset = previousSum;
    return childIndex;
}

FORCE_STATIC_INLINE void *child_containing_offset(HFBTreeNode *node, HFBTreeIndex offset, HFBTreeIndex * restrict outOffset, const BOOL isLeaf) {
    return node->children[index_containing_offset(node, offset, outOffset, isLeaf)];
}

FORCE_STATIC_INLINE ChildIndex_t index_for_child_at_offset(HFBTreeNode *node, HFBTreeIndex offset, const BOOL isLeaf) {
    ChildIndex_t childIndex;
    HFBTreeIndex previousSum = 0;
    void * const *children = node->children;
    for (childIndex = 0; childIndex < BTREE_ORDER; childIndex++) {
        if (previousSum == offset) break;
        HFASSERT(children[childIndex] != nil);
        HFBTreeIndex childLength = GET_LENGTH(children[childIndex], isLeaf);
        previousSum = HFSum(childLength, previousSum);
        HFASSERT(previousSum <= offset);
    }
    HFASSERT(childIndex <= BTREE_ORDER); //note we allow the child index to be one past the end (in which case we are sure to split the node)
    HFASSERT(previousSum == offset); //but we still require the offset to be the sum of all the lengths of this node
    return childIndex;
}

FORCE_STATIC_INLINE ChildIndex_t child_index_for_insertion_at_offset(HFBTreeBranch *branch, HFBTreeIndex insertionOffset, HFBTreeIndex *outPriorCombinedOffset) {
    ChildIndex_t indexForInsertion;
    HFBTreeIndex priorCombinedOffset = 0;
    void * const *children = branch->children;
    for (indexForInsertion = 0; indexForInsertion < BTREE_BRANCH_ORDER; indexForInsertion++) {
        if (! children[indexForInsertion]) break;
        const HFBTreeNode *childNode = children[indexForInsertion];
        HFBTreeIndex subtreeLength = childNode->subtreeLength;
        HFASSERT(subtreeLength > 0);
        HFBTreeIndex newOffset = HFSum(priorCombinedOffset, subtreeLength);
        if (newOffset >= insertionOffset) {
            break;
        }
        priorCombinedOffset = newOffset;
    }
    *outPriorCombinedOffset = priorCombinedOffset;
    return indexForInsertion;
}

FORCE_STATIC_INLINE ChildIndex_t child_index_for_deletion_at_offset(HFBTreeBranch *branch, HFBTreeIndex deletionOffset, HFBTreeIndex *outPriorCombinedOffset) {
    ChildIndex_t indexForDeletion;
    HFBTreeIndex priorCombinedOffset = 0;
    void * const *children = branch->children;
    for (indexForDeletion = 0; indexForDeletion < BTREE_BRANCH_ORDER; indexForDeletion++) {
        HFASSERT(branch->children[indexForDeletion] != nil);
        const HFBTreeNode *childNode = children[indexForDeletion];
        HFBTreeIndex subtreeLength = childNode->subtreeLength;
        HFASSERT(subtreeLength > 0);
        HFBTreeIndex newOffset = HFSum(priorCombinedOffset, subtreeLength);
        if (newOffset > deletionOffset) {
            /* Key difference between insertion and deletion: insertion uses >=, while deletion uses > */
            break;
        }
        priorCombinedOffset = newOffset;
    }
    *outPriorCombinedOffset = priorCombinedOffset;
    return indexForDeletion;
}

/* Given an array of pointers, insert a value into the array at the given insertionIndex, pushing every value at or above that index right by one. */
FORCE_STATIC_INLINE void insert_value_into_array(void *value, NSUInteger insertionIndex, void **array, NSUInteger arrayCount) {
    HFASSERT(insertionIndex <= arrayCount);
    HFASSERT(arrayCount > 0);
    NSUInteger pushingIndex = arrayCount - 1;
    while (pushingIndex > insertionIndex) {
        array[pushingIndex] = array[pushingIndex - 1];
        pushingIndex--;
    }
    array[insertionIndex] = value;
}

/* Given an array of pointers, remove the value at the given removalIndex, pulling every value at larger indexes left by one. Returns the removed pointer. */
FORCE_STATIC_INLINE void *remove_value_from_array(NSUInteger removalIndex, void **array, NSUInteger arrayCount) {
    HFASSERT(removalIndex < arrayCount);
    HFASSERT(arrayCount > 0);
    HFASSERT(array[removalIndex] != nil);
    void *result = array[removalIndex];
    for (NSUInteger pullingIndex = removalIndex + 1; pullingIndex < arrayCount; pullingIndex++) {
        array[pullingIndex - 1] = array[pullingIndex];
    }
    array[arrayCount - 1] = NULL;
    return result;
}

static void split_array(void * restrict * restrict values, ChildIndex_t valueCount, void ** restrict left, void ** restrict right, ChildIndex_t leftArraySizeForClearing) {
    const ChildIndex_t midPoint = valueCount/2;
    ChildIndex_t inputIndex = 0, outputIndex = 0;
    while (inputIndex < midPoint) {
        left[outputIndex++] = values[inputIndex++];
    }
    
    /* Clear the remainder of our left array.  Right array does not have to be cleared. */
    HFASSERT(outputIndex <= leftArraySizeForClearing);
    while (outputIndex < leftArraySizeForClearing) {
        left[outputIndex++] = nil;
    }
    
    /* Move the second half of our values into the right array */
    outputIndex = 0;
    while (inputIndex < valueCount) {
        right[outputIndex++] = values[inputIndex++];
    }
}

FORCE_STATIC_INLINE HFBTreeNode *add_child_to_node_possibly_creating_split(HFBTreeNode *node, void *value, ChildIndex_t insertionLocation, BOOL isLeaf) {
    ChildIndex_t childCount = count_node_values(node);
    HFASSERT(insertionLocation <= childCount);
    if (childCount < BTREE_ORDER) {
        /* No need to make a split */
        insert_value_into_array(value, insertionLocation, node->children, childCount + 1);
        node->subtreeLength = HFSum(node->subtreeLength, GET_LENGTH(value, isLeaf));
        return nil;
    }
    
    HFASSERT(node->children[BTREE_ORDER - 1] != nil); /* we require that it be full */
    void *allEntries[BTREE_ORDER + 1]; //this holds either TreeEntries or HFBTreeNodes
    memcpy(allEntries, node->children, BTREE_ORDER * sizeof *node->children);
    allEntries[BTREE_ORDER] = nil;
    
    /* Add this entry into the array. This can't overflow the array, since our local array is larger by 1. */
    insert_value_into_array(value, insertionLocation, allEntries, BTREE_ORDER + 1);
    HFBTreeNode *newNode = HFBTreeNode_allocate(isLeaf);
    
    /* figure out our total length */
    HFBTreeIndex totalLength = HFSum(node->subtreeLength, GET_LENGTH(value, isLeaf));
    
    /* Distribute half our values to the new leaf */
    split_array(allEntries, sizeof allEntries / sizeof *allEntries, node->children, newNode->children, BTREE_ORDER);
    
    /* figure out how much is in the new array */
    HFBTreeIndex newNodeLength = sum_child_lengths(newNode->children, isLeaf);
    
    /* update our lengths */
    HFASSERT(newNodeLength < totalLength);
    newNode->subtreeLength = newNodeLength;
    node->subtreeLength = totalLength - newNodeLength;
    
    /* Link it in */
    HFBTreeNode *rightNode = node->right;
    newNode->right = rightNode;
    if (rightNode) rightNode->left = newNode;
    newNode->left = node;
    node->right = newNode;
    
    /* Return the allocated node; the caller must take responsibility for freeing it */
    return newNode;
}

FORCE_STATIC_INLINE void add_values_to_array(void * const restrict * restrict srcValues, NSUInteger amountToCopy, void ** restrict targetValues, NSUInteger amountToPush) {
    // a pushed value at index X goes to index X + amountToCopy
    NSUInteger pushIndex = amountToPush;
    while (pushIndex--) {
        targetValues[amountToCopy + pushIndex] = targetValues[pushIndex];
    }
    for (NSUInteger i = 0; i < amountToCopy; i++) {
        targetValues[i] = srcValues[i];
    }
}

/* Remove values from the beginning of the array. */
FORCE_STATIC_INLINE void remove_values_from_array(void ** restrict array, NSUInteger amountToRemove, NSUInteger totalArrayLength) {
    HFASSERT(totalArrayLength >= amountToRemove);
    /* Move remaining values */
    NSUInteger i;
    for (i=amountToRemove; i < totalArrayLength; i++) {
        array[i - amountToRemove] = array[i];
    }
    /* Clear the end */
    for (i=totalArrayLength - amountToRemove; i < totalArrayLength; i++) {
        array[i] = NULL;
    }
}

/* Given a node, try to distribute all of its children to its neighbors, leaving the node empty. Returns YES if successful, NO if failed. */
FORCE_STATIC_INLINE BOOL rebalance_node_by_distributing_to_neighbors(HFBTreeNode *node, ChildIndex_t childCount, BOOL isLeaf, BOOL * restrict modifiedLeftNeighbor, BOOL *restrict modifiedRightNeighbor) {
    HFASSERT(childCount < BTREE_NODE_MINIMUM_VALUE_COUNT);
    BOOL result = NO;
    
    /* Count how much space is available to our left and right */
    HFBTreeNode *leftNeighbor = node->left, *rightNeighbor = node->right;
    const ChildIndex_t leftSpaceAvailable = (leftNeighbor ? BTREE_ORDER - count_node_values(leftNeighbor) : 0);
    const ChildIndex_t rightSpaceAvailable = (rightNeighbor ? BTREE_ORDER - count_node_values(rightNeighbor) : 0);
    
    if (leftSpaceAvailable + rightSpaceAvailable >= childCount) {
        /* We have enough space to redistribute.  Try to do it in such a way that both neighbors end up with the same number of items. */
        ChildIndex_t itemCountForLeft = 0, itemCountForRight = 0, itemCountRemaining = childCount;
        if (leftSpaceAvailable > rightSpaceAvailable) {
            ChildIndex_t amountForLeft = MIN(leftSpaceAvailable - rightSpaceAvailable, itemCountRemaining);
            itemCountForLeft += amountForLeft;
            itemCountRemaining -= amountForLeft;
        }
        else if (rightSpaceAvailable > leftSpaceAvailable) {
            ChildIndex_t amountForRight = MIN(rightSpaceAvailable - leftSpaceAvailable, itemCountRemaining);
            itemCountForRight += amountForRight;
            itemCountRemaining -= amountForRight;       
        }
        /* Now distribute the remainder (if any) evenly, preferring the remainder to go left, because it is slightly cheaper to append to the left than prepend to the right */
        itemCountForRight += itemCountRemaining / 2;
        itemCountForLeft += itemCountRemaining - (itemCountRemaining / 2);
        HFASSERT(itemCountForLeft <= leftSpaceAvailable);
        HFASSERT(itemCountForRight <= rightSpaceAvailable);
        HFASSERT(itemCountForLeft + itemCountForRight == childCount);
        
        if (itemCountForLeft > 0) {
            /* append to the end */
            HFBTreeIndex additionalLengthForLeft = sum_N_child_lengths(node->children, itemCountForLeft, isLeaf);
            leftNeighbor->subtreeLength = HFSum(leftNeighbor->subtreeLength, additionalLengthForLeft);
            add_values_to_array(node->children, itemCountForLeft, leftNeighbor->children + BTREE_ORDER - leftSpaceAvailable, 0);
            HFASSERT(leftNeighbor->subtreeLength == sum_child_lengths(leftNeighbor->children, isLeaf));
            *modifiedLeftNeighbor = YES;
        }
        if (itemCountForRight > 0) {
            /* append to the beginning */
            HFBTreeIndex additionalLengthForRight = sum_N_child_lengths(node->children + itemCountForLeft, itemCountForRight, isLeaf);
            rightNeighbor->subtreeLength = HFSum(rightNeighbor->subtreeLength, additionalLengthForRight);
            add_values_to_array(node->children + itemCountForLeft, itemCountForRight, rightNeighbor->children, BTREE_ORDER - rightSpaceAvailable);
            HFASSERT(rightNeighbor->subtreeLength == sum_child_lengths(rightNeighbor->children, isLeaf));
            *modifiedRightNeighbor = YES;
        }
        
        /* Remove ourself from the linked list, since we are going to die. */
        if (leftNeighbor) {
            leftNeighbor->right = rightNeighbor;
        }
        if (rightNeighbor) {
            rightNeighbor->left = leftNeighbor;
        }
        
        /* Even though we've essentially orphaned ourself, we need to force ourselves consistent (by making ourselves empty) because our parent still references us, and we don't want to make our parent inconsistent. */
        node->children[0] = NULL;
        node->subtreeLength = 0;
        
        result = YES;
    }
    return result;
}

/* Given a node with childCount children, and its neighbor, try to acquire some nodes from the neighbor so that everyone has at least BTREE_LEAF_MINIMUM_VALUE_COUNT nodes. */
FORCE_STATIC_INLINE BOOL share_children(HFBTreeNode *node, ChildIndex_t childCount, HFBTreeNode *neighbor, BOOL isRightNeighbor, BOOL isLeaf) {
    HFASSERT(count_node_values(node) == childCount);
    ChildIndex_t neighborCount = count_node_values(neighbor);
    ChildIndex_t totalChildren = (childCount + neighborCount);
    BOOL result = NO;
    if (totalChildren <= 2 * BTREE_LEAF_ORDER && totalChildren >= 2 * BTREE_LEAF_MINIMUM_VALUE_COUNT) {
        /* Determine how many we will have, and our neighbor will have. Round our count down (so we transfer fewer) */
        ChildIndex_t finalMyCount = totalChildren / 2;
        ChildIndex_t finalNeighborCount = totalChildren - finalMyCount;
        HFASSERT(finalNeighborCount < neighborCount);
        HFASSERT(finalMyCount > childCount);
        ChildIndex_t amountToTransfer = finalMyCount - childCount;
        
        HFBTreeIndex lengthChange;
        if (isRightNeighbor) {
            /* Transfer from left end of right neighbor to this right end of this node. */
            add_values_to_array(neighbor->children, amountToTransfer, node->children + childCount, 0);
            /* Remove from beginning of right neighbor. */
            remove_values_from_array(neighbor->children, amountToTransfer, neighborCount);
            lengthChange = sum_N_child_lengths(node->children + childCount, amountToTransfer, isLeaf);
        }
        else {
            /* Transfer from right end of left neighbor to left end of this node */
            add_values_to_array(neighbor->children + neighborCount - amountToTransfer, amountToTransfer, node->children, childCount);
            /* Remove from end of left neighbor */
            remove_values_from_array(neighbor->children + neighborCount - amountToTransfer, amountToTransfer, amountToTransfer);
            lengthChange = sum_N_child_lengths(node->children, amountToTransfer, isLeaf);
        }
        
        /* Fix up annotations */
        HFASSERT(lengthChange <= neighbor->subtreeLength);
        neighbor->subtreeLength -= lengthChange;
        node->subtreeLength = HFSum(node->subtreeLength, lengthChange);
        
        /* Ensure counts are what we expected */
        HFASSERT(count_node_values(node) == finalMyCount);
        HFASSERT(count_node_values(neighbor) == finalNeighborCount);
        
        /* We successfully rebalanced */
        result = YES;
    }
    return result;
}

/* Given a node, try to acquire some children from its neighbors, so that everyone has some leftover space. */
static BOOL rebalance_node_by_sharing_with_neighbors(HFBTreeNode *node, ChildIndex_t childCount, BOOL isLeaf, BOOL * restrict modifiedLeftNeighbor, BOOL *restrict modifiedRightNeighbor) {
    HFASSERT(childCount < BTREE_LEAF_MINIMUM_VALUE_COUNT);
    BOOL result = NO;
    HFBTreeNode *leftNeighbor = node->left, *rightNeighbor = node->right;
    if (leftNeighbor) {
        result = share_children(node, childCount, leftNeighbor, NO, isLeaf);
        if (result) *modifiedLeftNeighbor = YES;
    }
    if (! result && rightNeighbor) {
        result = share_children(node, childCount, rightNeighbor, YES, isLeaf);
        if (result) *modifiedRightNeighbor = YES;
    }
    return result;
}

/* Return YES if this leaf should be removed after rebalancing.  Other nodes are never removed. */
FORCE_STATIC_INLINE BOOL rebalance_node_after_deletion(HFBTreeNode *node, ChildIndex_t childCount, BOOL isLeaf, BOOL * restrict modifiedLeftNeighbor, BOOL *restrict modifiedRightNeighbor) {
    HFASSERT(childCount < BTREE_LEAF_MINIMUM_VALUE_COUNT);
    HFASSERT(childCount == count_node_values(node));
    
    /* We may only delete this leaf, and not adjacent leaves.  Thus our rebalancing strategy is:
     If the items to the left or right have sufficient space to hold us, then push our values left or right, and delete this node.
     Otherwise, steal items from the left until we have the same number of items. */
    BOOL deleteNode = NO;
    if (rebalance_node_by_distributing_to_neighbors(node, childCount, isLeaf, modifiedLeftNeighbor, modifiedRightNeighbor)) {
        deleteNode = YES;
        //puts("rebalance_node_by_distributing_to_neighbors");
    }
    else if (rebalance_node_by_sharing_with_neighbors(node, childCount, isLeaf, modifiedLeftNeighbor, modifiedRightNeighbor)) {
        deleteNode = NO;
        //puts("rebalance_node_by_sharing_with_neighbors");
    }
    else {
        [NSException raise:NSInternalInconsistencyException format:@"Unable to rebalance after deleting node %@", node];
    }
    return deleteNode;
}

/* Counts how many times the given value is in the children of the given node */
__attribute__((unused))
static NSUInteger count_child_multiplicity(HFBTreeNode *node, void *value) {
    NSUInteger result = 0;
    for (ChildIndex_t idx = 0; idx < BTREE_ORDER; idx++) {
        void *child = node->children[idx];
        if (! child) break;
        if (child == value) result++;
    }
    return result;
}

/* Remove a value at the given index from the given node. */
FORCE_STATIC_INLINE BOOL remove_value_from_node_with_possible_rebalance(HFBTreeNode *node, ChildIndex_t childIndex, BOOL isRootNode, BOOL isLeaf, BOOL * restrict modifiedLeftNeighbor, BOOL *restrict modifiedRightNeighbor) {
    HFASSERT(childIndex < BTREE_ORDER);
    HFASSERT(node != nil);
    HFASSERT(node->children[childIndex] != nil);
    HFBTreeIndex entryLength = GET_LENGTH(node->children[childIndex], isLeaf);
    HFASSERT(entryLength <= node->subtreeLength);
    node->subtreeLength -= entryLength;
    BOOL deleteInputNode = NO;
    
    
#if ! NDEBUG
    /* Testing code to verify that the number of children in the array is reduced by 1. */
    void *childValue = node->children[childIndex];    
    NSUInteger beforeChildMultiplicity = count_child_multiplicity(node, childValue);
#endif
    
    /* Figure out how many children we have; start at one more than childIndex since we know that childIndex is a valid index */
    ChildIndex_t childCount;
    for (childCount = childIndex + 1; childCount < BTREE_ORDER; childCount++) {
        if (! node->children[childCount]) break;
    }
    HFASSERT(childCount == count_node_values(node));
    
    /* Remove our value at childIndex. This is either a pointer to TreeEntry_t (if we're a leaf) or to a HFBTreeNode (if we're a branch)  */
    void *removedChild = remove_value_from_array(childIndex, node->children, childCount);
    HFASSERT(childCount > 0);
    childCount--;
    
    /* If we are a branch, then deallocate this removed child (which is a HFBTreeNode *. If we are a leaf, then the caller will take responsibility. Yuck, it sure would be nice to factor this better. */
    if (! isLeaf) {
        HFBTreeNode_free(removedChild);
    }
    
#if ! NDEBUG
    /* Testing code to verify that the number of children in the array is reduced by 1. */
    NSUInteger afterChildMultiplicity = count_child_multiplicity(node, childValue);
    HFASSERT(beforeChildMultiplicity == afterChildMultiplicity + 1);
#endif

    if (childCount < BTREE_LEAF_MINIMUM_VALUE_COUNT && ! isRootNode) {
        /* We have too few items; try to rebalance (this will always be possible except from the root node) */
        deleteInputNode = rebalance_node_after_deletion(node, childCount, isLeaf, modifiedLeftNeighbor, modifiedRightNeighbor);
    }
    else {
        //NSLog(@"Deletion from %@ with %u remaining, %s root node, so no need to rebalance\n", node, childCount, isRootNode ? "is" : "is not");
    }
    
    return deleteInputNode;
}

FORCE_STATIC_INLINE void update_node_having_changed_size_of_child(HFBTreeNode *node, BOOL isLeaf) {
    HFBTreeIndex newLength = sum_child_lengths(node->children, isLeaf);
    /* This should only be called if the length actually changes - so assert as such */
    /* I no longer think the above line is true.  It's possible that we can delete a node, and then after a rebalance, we can become the same size we were before. */
    //HFASSERT(node->subtreeLength != newLength);
    node->subtreeLength = newLength;
}

struct SubtreeInfo_t {
    HFBTreeBranch *branch;
    ChildIndex_t childIndex; //childIndex is the index of the child of branch, not branch's index in its parent
};

static HFBTreeLeaf *btree_descend(HFBTree *tree, struct SubtreeInfo_t *outDescentInfo, HFBTreeIndex *insertionOffset, BOOL isForDelete) {
    TreeDepth_t maxDepth = tree->depth;
    HFASSERT(maxDepth != BAD_DEPTH && maxDepth <= MAX_DEPTH);
    
    HFBTreeNode *currentBranchOrLeaf = tree->root;
    HFBTreeIndex offsetForSubtree = *insertionOffset;
    
    /* Walk down the tree */
    for (TreeDepth_t currentDepth = 0; currentDepth < maxDepth; currentDepth++) {
        ASSERT_IS_BRANCH(currentBranchOrLeaf);
        HFBTreeBranch *currentBranch = currentBranchOrLeaf;
        HFBTreeIndex priorCombinedOffset = -1;
        
        ChildIndex_t nextChildIndex = (isForDelete ? child_index_for_deletion_at_offset : child_index_for_insertion_at_offset)(currentBranch, offsetForSubtree, &priorCombinedOffset);
        
        /* Record information about our descent */
        outDescentInfo[currentDepth].branch = currentBranch;
        outDescentInfo[currentDepth].childIndex = nextChildIndex;
        
        offsetForSubtree -= priorCombinedOffset;
        currentBranchOrLeaf = currentBranch->children[nextChildIndex];
    }
    ASSERT_IS_LEAF(currentBranchOrLeaf);
    *insertionOffset = offsetForSubtree;
    return currentBranchOrLeaf;
}

struct LeafInfo_t {
    HFBTreeLeaf *leaf;
    ChildIndex_t entryIndex;
    HFBTreeIndex offsetOfEntryInTree;
};

static struct LeafInfo_t btree_find_leaf(HFBTree *tree, HFBTreeIndex offset) {
    TreeDepth_t depth = tree->depth;
    HFBTreeNode *currentNode = tree->root;
    HFBTreeIndex remainingOffset = offset;
    while (depth--) {
        HFBTreeIndex beginningOffsetOfNode;
        currentNode = child_containing_offset(currentNode, remainingOffset, &beginningOffsetOfNode, NO);
        HFASSERT(beginningOffsetOfNode <= remainingOffset);
        remainingOffset = remainingOffset - beginningOffsetOfNode;
    }
    ASSERT_IS_LEAF(currentNode);
    HFBTreeIndex startOffsetOfEntry;
    ChildIndex_t entryIndex = index_containing_offset(currentNode, remainingOffset, &startOffsetOfEntry, YES);
    /* The offset of this entry is the requested offset minus the difference between its starting offset within the leaf and the requested offset within the leaf */
    HFASSERT(remainingOffset >= startOffsetOfEntry);
    HFBTreeIndex offsetIntoEntry = remainingOffset - startOffsetOfEntry;
    HFASSERT(offset >= offsetIntoEntry);
    HFBTreeIndex beginningOffset = offset - offsetIntoEntry;
    ASSERT_IS_LEAF(currentNode);
    return (struct LeafInfo_t){.leaf = currentNode, .entryIndex = entryIndex, .offsetOfEntryInTree = beginningOffset};
}

static TreeEntry_t *btree_search(HFBTree *tree, HFBTreeIndex offset, HFBTreeIndex *outBeginningOffset) {
    struct LeafInfo_t leafInfo = btree_find_leaf(tree, offset);
    *outBeginningOffset = leafInfo.offsetOfEntryInTree;
    return leafInfo.leaf->children[leafInfo.entryIndex];
}

static HFBTreeNode *btree_insert_returning_value_for_parent(HFBTree *tree, TreeEntry_t *entry, HFBTreeIndex insertionOffset) {
    struct SubtreeInfo_t descentInfo[MAX_DEPTH];
#if ! NDEBUG
    memset(descentInfo, -1, sizeof descentInfo);
#endif
    HFBTreeIndex subtreeOffset = insertionOffset;
    HFBTreeLeaf *leaf = btree_descend(tree, descentInfo, &subtreeOffset, NO);
    ASSERT_IS_LEAF(leaf);
    
    /* Insert into the tree. This may allocate a new node. */
    ChildIndex_t insertionLocation = index_for_child_at_offset(leaf, subtreeOffset, YES);
    HFBTreeNode *allocatedNodeToInsertIntoParentBranch = add_child_to_node_possibly_creating_split(leaf, entry, insertionLocation, YES);
    
    /* Walk up */
    TreeDepth_t depth = tree->depth;
    HFASSERT(depth != BAD_DEPTH);
    HFBTreeIndex entryLength = HFBTreeLength(entry);
    while (depth--) {
        HFBTreeBranch *branch = descentInfo[depth].branch;
        branch->subtreeLength = HFSum(branch->subtreeLength, entryLength);
        ChildIndex_t childIndex = descentInfo[depth].childIndex;
        if (allocatedNodeToInsertIntoParentBranch) {
            /* Since we copied some stuff out from under ourselves, subtract its length. */
            HFASSERT(branch->subtreeLength >= allocatedNodeToInsertIntoParentBranch->subtreeLength);
            branch->subtreeLength -= allocatedNodeToInsertIntoParentBranch->subtreeLength;
            
            /* Insert the newly allocated node. This may cause US to split, generating a new node for OUR parent. */
            HFBTreeNode *newAllocatedNodeToInsertIntoParentBranch = add_child_to_node_possibly_creating_split(branch, allocatedNodeToInsertIntoParentBranch, childIndex + 1, NO);
            
            allocatedNodeToInsertIntoParentBranch = newAllocatedNodeToInsertIntoParentBranch;
        }
    }
    
    /* And return this all the way to root */
    return allocatedNodeToInsertIntoParentBranch;
}

static BOOL btree_remove(HFBTree *tree, HFBTreeIndex deletionOffset, TreeEntry_t **outRemovedEntry) {
    /* Descend into the tree */
    struct SubtreeInfo_t descentInfo[MAX_DEPTH];
#if ! NDEBUG
    memset(descentInfo, -1, sizeof descentInfo);
#endif
    HFBTreeIndex subtreeOffset = deletionOffset;
    HFBTreeLeaf *leaf = btree_descend(tree, descentInfo, &subtreeOffset, YES);
    ASSERT_IS_LEAF(leaf);
    
    /* Get the index of the child whose offset matches subtreeOffset exactly. */
    HFBTreeIndex previousOffsetSum = 0;
    ChildIndex_t childIndex;
    for (childIndex = 0; childIndex < BTREE_LEAF_ORDER; childIndex++) {
        if (previousOffsetSum == subtreeOffset) break;
        TreeEntry_t *entry = leaf->children[childIndex];
        HFASSERT(entry != nil); //if it were nil, then the offset is too large
        HFBTreeIndex childLength = HFBTreeLength(entry);
        previousOffsetSum = HFSum(childLength, previousOffsetSum);
    }
    HFASSERT(childIndex < BTREE_LEAF_ORDER);
    HFASSERT(previousOffsetSum == subtreeOffset);
    
    /* Return the removed entry to the parent. Note that remove_value_from_node_with_possible_rebalance is careful to not free this object (it only frees nodes) */
    HFASSERT(leaf->children[childIndex] != NULL);
    *outRemovedEntry = leaf->children[childIndex];
    
    /* Remove the child from the node. */
    TreeDepth_t depth = tree->depth;
    HFASSERT(depth != BAD_DEPTH);
    BOOL modifiedLeft = NO, modifiedRight = NO;
    BOOL deleteNode = remove_value_from_node_with_possible_rebalance(leaf, childIndex, tree->depth == 0/*isRootNode*/, YES /*isLeaf*/, &modifiedLeft, &modifiedRight);
    
    /* Maybe we need to delete this node! */
    HFASSERT(btree_are_cached_lengths_correct(leaf, 0, NULL));
    while (depth--) {
        HFBTreeBranch *branch = descentInfo[depth].branch;
        ChildIndex_t branchChildIndex = descentInfo[depth].childIndex;
        
        /* If our child modified the node count of its left neighbor, and its left neighbor is not also a child of us, we need to inform its parent (which is our left neighbor) */
        BOOL leftNeighborNeedsUpdating = modifiedLeft && branchChildIndex == 0;
        
        /* If our child modified the node count of its right neighbor, and its right neighbor is not also a child of us, we need to inform its parent (which is our right neighbor) */
        BOOL rightNeighborNeedsUpdating = modifiedRight && (branchChildIndex + 1 == BTREE_BRANCH_ORDER || branch->children[branchChildIndex + 1] == NULL);
        
        if (leftNeighborNeedsUpdating) {
            HFASSERT(branch->left != NULL);
            //NSLog(@"Updating lefty %p", branch->left);
            update_node_having_changed_size_of_child(branch->left, NO);
        }
        if (rightNeighborNeedsUpdating) {
            HFASSERT(branch->right != NULL);
            //NSLog(@"Updating righty %p", branch->right);
            update_node_having_changed_size_of_child(branch->right, NO);
        }
#if ! NDEBUG
        if (branch->left) HFASSERT(btree_are_cached_lengths_correct(branch->left, tree->depth - depth, NULL));
        if (branch->right) HFASSERT(btree_are_cached_lengths_correct(branch->right, tree->depth - depth, NULL));
#endif  
        /* And always update the parent */
        update_node_having_changed_size_of_child(branch, NO);
        
        /* Now delete the child if necessary */
        modifiedLeft = modifiedRight = NO;
        if (deleteNode) {
            deleteNode = remove_value_from_node_with_possible_rebalance(branch, branchChildIndex, depth==0/*isRootNode*/, NO, &modifiedLeft, &modifiedRight);
        }
        else {
            /* No need to delete parent nodes, so leave deleteNode as NO */
        }
        
        /* If we had to modify our left or right neighbor, our parent may also have to modify its left or right neighbor, if one of our children modified a neighbor that is not also a child of us. Deleting sure is complicated. */
        modifiedLeft = modifiedLeft || leftNeighborNeedsUpdating;
        modifiedRight = modifiedRight || rightNeighborNeedsUpdating;
    }
    
    if (! deleteNode) {
        if (tree->depth == 0) {
            /* Delete the root if it is now entirely emtpy */
            deleteNode = (tree->root->children[0] == NULL);
        } else {
            /* We expect the root to have at least one node in this case. */
            HFASSERT(tree->root->children[0] != NULL);
            /* Delete the root if it has one node */
            deleteNode = (tree->root->children[1] == NULL);
        }
    }
    return deleteNode;
}

/* linkingHelper stores the last seen node for each depth.  */
static HFBTreeNode *mutable_copy_node(HFBTreeNode *node, TreeDepth_t depth, HFBTreeNode **linkingHelper) {
    if (node == nil) return nil;
    HFASSERT(depth != BAD_DEPTH);
    BOOL isLeaf = (depth == 0);
    HFBTreeNode *result = HFBTreeNode_allocate(isLeaf);
    result->subtreeLength = node->subtreeLength;
    
    /* Link us in */
    HFBTreeNode *leftNeighbor = linkingHelper[0];
    if (leftNeighbor != nil) {
        leftNeighbor->right = result;
        result->left = leftNeighbor;
    }
    
    /* Leave us for our future right neighbor to find */
    linkingHelper[0] = (void *)result;
    
    HFBTreeIndex index;
    for (index = 0; index < BTREE_ORDER; index++) {
        void *child = node->children[index];
        if (! child) break;
        if (depth > 0) {
            result->children[index] = mutable_copy_node(child, depth - 1, linkingHelper + 1);
        }
        else {
            result->children[index] = (void *)CFRetain(child);
        }
    }
    return result;
}

__attribute__((unused))
static BOOL non_nulls_are_grouped_at_start(void * const *ptr, NSUInteger count) {
    BOOL hasSeenNull = NO;
    for (NSUInteger i=0; i < count; i++) {
        BOOL ptrIsNull = (ptr[i] == nil);
        hasSeenNull = hasSeenNull || ptrIsNull;
        if (hasSeenNull && ! ptrIsNull) {
            return NO;
        }
    }
    return YES;
}


static void btree_recursive_check_integrity(HFBTree *tree, HFBTreeNode *branchOrLeaf, TreeDepth_t depth, HFBTreeNode **linkHelper) {
    HFASSERT(linkHelper[0] == branchOrLeaf->left);
    if (linkHelper[0]) HFASSERT(linkHelper[0]->right == branchOrLeaf);
    linkHelper[0] = branchOrLeaf;
    
    if (depth == 0) {
        ASSERT_IS_LEAF(branchOrLeaf);
        HFBTreeLeaf *leaf = branchOrLeaf;
        HFASSERT(non_nulls_are_grouped_at_start(leaf->children, BTREE_LEAF_ORDER));
    }
    else {
        ASSERT_IS_BRANCH(branchOrLeaf);
        HFBTreeBranch *branch = branchOrLeaf;
        HFASSERT(non_nulls_are_grouped_at_start(branch->children, BTREE_BRANCH_ORDER));
        for (ChildIndex_t i = 0; i < BTREE_BRANCH_ORDER; i++) {
            if (! branch->children[i]) break;
            btree_recursive_check_integrity(tree, branch->children[i], depth - 1, linkHelper + 1);
        }
    }
    ChildIndex_t childCount = count_node_values(branchOrLeaf);
    if (depth < tree->depth) { // only the root may have fewer than BTREE_NODE_MINIMUM_VALUE_COUNT
        HFASSERT(childCount >= BTREE_NODE_MINIMUM_VALUE_COUNT);
    }
    HFASSERT(childCount <= BTREE_ORDER);
}

static BOOL btree_are_cached_lengths_correct(HFBTreeNode *branchOrLeaf, TreeDepth_t depth, HFBTreeIndex *outLength) {
    if (! branchOrLeaf) {
        if (outLength) *outLength = 0;
        return YES;
    }
    HFBTreeIndex length = 0;
    if (depth == 0) {
        ASSERT_IS_LEAF(branchOrLeaf);
        HFBTreeLeaf *leaf = branchOrLeaf;
        for (ChildIndex_t i=0; i < BTREE_LEAF_ORDER; i++) {
            if (! leaf->children[i]) break;
            length = HFSum(length, HFBTreeLength(leaf->children[i]));
        }
    }
    else {
        ASSERT_IS_BRANCH(branchOrLeaf);
        HFBTreeBranch *branch = branchOrLeaf;
        for (ChildIndex_t i=0; i < BTREE_BRANCH_ORDER; i++) {
            if (! branch->children[i]) break;
            HFBTreeIndex subLength = -1;
            if (! btree_are_cached_lengths_correct(branch->children[i], depth - 1, &subLength)) {
                return NO;
            }
            length = HFSum(length, subLength);
        }
    }
    if (outLength) *outLength = length;
    return length == branchOrLeaf->subtreeLength;
}

static NSUInteger btree_entry_count(HFBTree *tree) {
    __block NSUInteger result = 0;
    walk_tree_nodes(tree, ^(HFBTreeNode *leaf, BOOL isLeaf) {
        if (isLeaf) {
            for (ChildIndex_t i=0; i < BTREE_LEAF_ORDER; i++) {
                if (! leaf->children[i]) break;
                result++;
            }
        }
    });
    return result;
}

static void btree_check_cached_annotations(HFBTree *tree) {
    walk_tree_nodes(tree, ^(HFBTreeNode *node, BOOL isLeaf) {
        /* We depend on the fact that our children are processed before the node, so our child annotations have already been fixed up. */
        HFBTreeIndex subtreeLength = 0;
        for (ChildIndex_t i = 0; i < BTREE_LEAF_ORDER; i++) {
            void *child = node->children[i];
            if (! child) break;
            subtreeLength = HFSum(subtreeLength, GET_LENGTH(child, isLeaf));
        }
        HFASSERT(node->subtreeLength == subtreeLength);
    });
}

static void btree_fixup_cached_annotations(HFBTree *tree) {
    walk_tree_nodes(tree, ^(HFBTreeNode *node, BOOL isLeaf) {
        /* We depend on the fact that our children are processed before the node, so our child annotations have already been fixed up. */
        HFBTreeIndex subtreeLength = 0;
        for (ChildIndex_t i = 0; i < BTREE_LEAF_ORDER; i++) {
            void *child = node->children[i];
            if (! child) break;
            subtreeLength = HFSum(subtreeLength, GET_LENGTH(child, isLeaf));
        }
        node->subtreeLength = subtreeLength;
    });
}

FORCE_STATIC_INLINE void btree_apply_to_entries(HFBTree *tree, HFBTreeIndex offset, BOOL (^blk)(id entry, HFBTreeIndex idx)) {
    /* Find the leaf to start at */
    struct LeafInfo_t leafInfo = btree_find_leaf(tree, offset);
    HFBTreeLeaf *leaf = leafInfo.leaf;
    ChildIndex_t entryIndex = leafInfo.entryIndex;
    HFBTreeIndex leafOffset = leafInfo.offsetOfEntryInTree;
    
    BOOL continueApplying = YES;
    while (leaf != NULL) {
        /* Walk over the entries in this leaf */
        for (; entryIndex < BTREE_LEAF_ORDER; entryIndex++) {
            
            /* Get the next entry; if it's NULL then we're with this leaf */
            TreeEntry_t *entry = leaf->children[entryIndex];
            if (! entry) break;
            
            /* Invoke the block */
            continueApplying = blk((__bridge id)entry, leafOffset);
            if (! continueApplying) break;
            leafOffset = HFSum(leafOffset, HFBTreeLength(entry));
        }
        if (! continueApplying) break;
        
        /* Go to the next leaf */
        leaf = leaf->right;
        if (leaf) ASSERT_IS_LEAF(leaf);
        entryIndex = 0;
    }
}

FORCE_STATIC_INLINE void btree_apply_function_to_entries(HFBTree *tree, HFBTreeIndex offset, BOOL (*func)(id, HFBTreeIndex, void *), void *userInfo) {
    btree_apply_to_entries(tree, offset, ^BOOL(id entry, HFBTreeIndex idx) {
        return func(entry, idx, userInfo);
    });
}

- (NSEnumerator *)entryEnumerator {
    if (! root) return [[NSArray array] objectEnumerator];
    __block HFBTreeLeaf *currentLeaf = btree_find_leaf(self, 0).leaf;
    __block ChildIndex_t childIndex = 0;
    return [HFEnumerator enumeratorWithBlock:^id{
        if (! currentLeaf) return nil;
        if (childIndex >= BTREE_LEAF_ORDER || currentLeaf->children[childIndex] == nil) {
            childIndex = 0;
            currentLeaf = currentLeaf->right;
            if (currentLeaf) ASSERT_IS_LEAF(currentLeaf);
        }
        if (currentLeaf == nil) return nil;
        HFASSERT(currentLeaf->children[childIndex] != nil);
        TreeEntry_t *entry = currentLeaf->children[childIndex++];
        return (__bridge id)entry;
    }];
}

- (NSArray *)allEntries {
    if (! root) return [NSArray array];
    NSUInteger treeCapacity = 1;
    unsigned int depthIndex = depth;
    while (depthIndex--) treeCapacity *= BTREE_ORDER;
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:treeCapacity/2]; //assume we're half full
    btree_apply_to_entries(self, 0, ^BOOL(id entry, HFBTreeIndex idx) {
        [result addObject:entry];
        return YES;
    });
    return result;
}

- (void)applyFunction:(BOOL (*)(id entry, HFBTreeIndex offset, void *userInfo))func toEntriesStartingAtOffset:(HFBTreeIndex)offset withUserInfo:(void *)userInfo {
    NSParameterAssert(func != NULL);
    if (! root) return;
    btree_apply_function_to_entries(self, offset, func, userInfo);
}

@end
