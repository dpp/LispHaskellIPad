//
//  PerformOMatic.m
//  Lisp
//
//  Created by David Pollak on 6/17/11.
//  Copyright 2011 lift Web Framework. All rights reserved.
//

#import "PerformOMatic.h"


@implementation PerformOMatic
- (void)run {
	[self performSelectorOnMainThread:@selector(reallyDoIt:) withObject:self waitUntilDone:TRUE];
}

- (void)reallyDoIt:(id)ignore {
	whatToDo(NULL);
	releaseMe(whatToDo);
	[self dealloc];
}

- (void)setFunc:(void *)func {
	whatToDo = func;
}

@end
