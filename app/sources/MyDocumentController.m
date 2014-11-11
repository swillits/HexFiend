//
//  MyDocumentController.m
//  HexFiend_2
//
//  Copyright 2010 ridiculous_fish. All rights reserved.
//

#import "MyDocumentController.h"
#import "BaseDataDocument.h"
#include <sys/stat.h>

@implementation MyDocumentController

- (void)noteNewRecentDocumentURL:(NSURL *)absoluteURL {
    /* Work around the fact that LS crashes trying to fetch icons for block and character devices.  Let's just prevent it for all files that aren't normal or directories, heck. */
    BOOL callSuper = YES;
    unsigned char path[PATH_MAX + 1];
    struct stat sb;
    if (absoluteURL && CFURLGetFileSystemRepresentation((CFURLRef)absoluteURL, YES, path, sizeof path) && 0 == stat((char *)path, &sb)) {
        if (! S_ISREG(sb.st_mode) && ! S_ISDIR(sb.st_mode)) {
            callSuper = NO;
        }
    }
    if (callSuper) {
        [super noteNewRecentDocumentURL:absoluteURL];
    }
}

- (BaseDataDocument *)transientDocumentToReplace {
    BaseDataDocument *result = nil;
    NSArray *documents = [self documents];
    if ([documents count] == 1) {
        BaseDataDocument *potentialResult = documents[0];
        if ([potentialResult respondsToSelector:@selector(isTransientAndCanBeReplaced)] && [potentialResult isTransientAndCanBeReplaced]) {
            result = potentialResult;
        }
    }
    return result;
}

- (void)displayDocument:(NSDocument *)doc {
    // Documents must be displayed on the main thread.
    if ([NSThread isMainThread]) {
        [doc makeWindowControllers];
        [doc showWindows];
    } else {
        [self performSelectorOnMainThread:_cmd withObject:doc waitUntilDone:YES];
    }
}

- (void)replaceTransientDocument:(NSArray *)documents {
    // Transient document must be replaced on the main thread, since it may undergo automatic display on the main thread.
    if ([NSThread isMainThread]) {
        BaseDataDocument *transientDoc = documents[0], *doc = documents[1];
        NSArray *controllersToTransfer = [[transientDoc windowControllers] copy];
        FOREACH(NSWindowController *, controller, controllersToTransfer) {
            [doc addWindowController:controller];
            [transientDoc removeWindowController:controller];
            [doc adoptWindowController:controller fromTransientDocument:transientDoc];
        }
        [transientDoc close];
        [controllersToTransfer release];
        
    } else {
        [self performSelectorOnMainThread:_cmd withObject:documents waitUntilDone:YES];
    }
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-implementations"
- (id)openDocumentWithContentsOfURL:(NSURL *)absoluteURL display:(BOOL)displayDocument error:(NSError **)outError {
#pragma clang diagnostic pop
    BaseDataDocument *transientDoc = [self transientDocumentToReplace];
    
    // Don't make NSDocumentController display the NSDocument it creates. Instead, do it later manually to ensure that the transient document has been replaced first.
    BaseDataDocument *result = [super openDocumentWithContentsOfURL:absoluteURL display:NO error:outError];
    if (result) {
        if ([result isKindOfClass:[BaseDataDocument class]] && transientDoc) {
            [transientDoc setTransient:NO];
            [self replaceTransientDocument:@[transientDoc, result]];
        }
        if (displayDocument) [self displayDocument:result];
    }
    
    return result;
}

- (id)openUntitledDocumentAndDisplay:(BOOL)displayDocument error:(NSError **)outError {
    BaseDataDocument *doc = [super openUntitledDocumentAndDisplay:displayDocument error:outError];
    if ([doc respondsToSelector:@selector(setTransient:)]) {
        [doc setTransient:YES];
    }
    return doc;
}

@end
