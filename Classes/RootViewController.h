//
//  RootViewController.h
//  Lisp
//
//  Created by David Pollak on 6/7/11.
//  Copyright 2011 lift Web Framework. All rights reserved.
//

#import <UIKit/UIKit.h>

@class DetailViewController;

@interface RootViewController : UITableViewController {
    DetailViewController *detailViewController;
}

@property (nonatomic, retain) IBOutlet DetailViewController *detailViewController;

@end
