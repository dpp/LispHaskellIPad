//
//  PerformOMatic.h
//  Lisp
//
//  Created by David Pollak on 6/17/11.
//  Copyright 2011 lift Web Framework. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface PerformOMatic : NSObject {
	void (*whatToDo)(void *);
	
}

- (void)setFunc:(void *)func;
- (void)run;
- (void)reallyDoIt:(id)ignore;
@end

extern void releaseMe(void *);